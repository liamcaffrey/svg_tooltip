# install.packages("RODBC", dependencies = TRUE)
# install.packages("RODBCext", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)
# install.packages("dplyr", dependencies = TRUE)
# install.packages("tidyr", dependencies = TRUE)
# install.packages("lubridate", dependencies = TRUE)
# install.packages("plotly", dependencies = TRUE)
# install.packages("ggthemes", dependencies = TRUE)
# install.packages("RCurl", dependencies = TRUE)

rm(list = ls())

library(RODBC)
library(RODBCext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(ggthemes)
library(RCurl)

CalendarDate <- c(as.character(ymd(format(Sys.time(), format="%Y-%m-%d"))))

df.facet_data_unpivot <- dget("TooltipNotWorkingData.txt")

png_pixel_width <- 1600
png_pixel_height <- 900
axis_font_size <- 10
title_font_size <- 18
x_axis_label <- "Calendar Date"

min_date <- min(as.Date(df.facet_data_unpivot$calendar_date))
max_date <- max(as.Date(df.facet_data_unpivot$calendar_date))

ggplot_by_vehicle <- function(p_facet_category, p_facet_wrap_column_name, p_subset_column_name, p_snr_chart_category, p_snr_set_constraints, p_snr_miss_condition)
{
	facet_category <- p_facet_category
	facet_wrap_column_name <- p_facet_wrap_column_name
	subset_column_name <- p_subset_column_name
	snr_chart_category <- p_snr_chart_category
	snr_set_constraints <- p_snr_set_constraints
	snr_miss_condition <- p_snr_miss_condition

	print(paste("facet_category", paste("'", facet_category, "'", sep = ""), sep = " <- "))
	print(paste("facet_wrap_column_name", paste("'", facet_wrap_column_name, "'", sep = ""), sep = " <- "))
	print(paste("subset_column_name", paste("'", subset_column_name, "'", sep = ""), sep = " <- "))
	print(paste("snr_chart_category", paste("'", snr_chart_category, "'", sep = ""), sep = " <- "))
	print(paste("snr_set_constraints", paste("'", snr_set_constraints, "'", sep = ""), sep = " <- "))
	print(paste("snr_miss_condition", paste("'", snr_miss_condition, "'", sep = ""), sep = " <- "))

	snr_axis_scale_type <- "Log"

	print(paste("Generating", snr_chart_category, facet_category, snr_axis_scale_type, format(max_date, format="%Y-%m-%d")))
	# facet charts usually have a 'm' x 'm', e.g. 6 x 6 or 4 x 4. Work out how many based on the number of facets in the target set.
	number_of_facet_columns <- ceiling(sqrt(length(unique(df.facet_data_unpivot[[facet_wrap_column_name]]))))

	df.data_chart_category <- na.omit(filter(df.facet_data_unpivot, df.facet_data_unpivot[[subset_column_name]] == snr_chart_category))
	if (nrow(df.data_chart_category) == 0) {
		print("   No rows matching criteria")
	} else {
		latest_date <- max(ymd_hms(df.data_chart_category$LatestTime))

		snr_y_axis_label <- paste("\n", snr_chart_category, " Signal-to-Noise Ratio (SNR) expressed as Rate/'000 - ", snr_axis_scale_type, " scale", sep="")
		snr_chart_title <- paste("SNR (", snr_chart_category, " - ", snr_axis_scale_type, " scale) - last 10 workdays as of ", format(latest_date, format="%Y-%m-%d %H:%M"), sep="")
		snr_chart_caption <-  paste("Chart generated on : ", format(Sys.time(), format="%Y-%m-%d %H:%M"), sep="")
		if (facet_category == "ByCategory") {
			snr_chart_subtitle <- ""
		} else {
			snr_chart_subtitle <- paste("Context for chart... set constraints: ", snr_set_constraints, "; condition to count misses: ", snr_miss_condition, sep="")
		}

		# Remove spaces from the SNR chart category part of the filename
		snr_filename <- paste(paste("TooltipNotWorking", c(as.character(format(Sys.time(), format="%Y-%m-%d_%H%M%S"))), gsub(" ", "", snr_chart_category), "SNR", snr_axis_scale_type, sep="_"), "html", sep=".")
		snr_filename_png <- paste(paste("TooltipNotWorking", c(as.character(format(Sys.time(), format="%Y-%m-%d_%H%M%S"))), gsub(" ", "", snr_chart_category), "SNR", snr_axis_scale_type, sep="_"), "png", sep=".")

		print(snr_filename)
		p <- ggplot(data=df.data_chart_category
				   ,aes(x=calendar_day, y=rate_per_thousand, group=key, colour=factor(key))
				   # http://stackoverflow.com/questions/5106782/use-of-ggplot-within-another-function-in-r
				   ,environment = environment()
			) +
		    geom_point(aes(size = bubble_size, text = sprintf("Rate/'000: %s<br>Misses: %s<br>Hits: %s<br>Total/Day: %s", rate_per_thousand, misses, hits, total_date_count)), alpha=0.4) +
			#geom_point(aes(size = bubble_size), alpha=0.4) +
			scale_size(name = "Target Cnt", breaks = c(10, 50, 100, 500, 800, 1000), range = c(1,12)) +
			scale_color_manual(name = "Key Type", values = c("Key1" = "red", "Key2" = "green", "Key3" = "lightblue", "Key4" = "purple", "NoKey" = "cyan", "Key0" = "darkgrey")) +
			guides(colour = guide_legend(override.aes = list(size=10))) +
			facet_wrap(as.formula(paste("~", facet_wrap_column_name)), ncol=number_of_facet_columns) +
			labs(x = x_axis_label, y = snr_y_axis_label, title = snr_chart_title, subtitle = snr_chart_subtitle, caption = snr_chart_caption, color="KeyName") +
			theme_few() +
			theme(axis.text.x = element_text(angle=90, vjust=-0.01)) + 
			theme(axis.title = element_text(size = axis_font_size)) + 
			theme(plot.title = element_text(color = '#666666', 
									  face = 'bold',
									  size = title_font_size,
									  hjust = 0))
									  
		if (snr_axis_scale_type == "Log") {
		  p <- p +
			scale_y_log10() #+
			#annotation_logticks(sides="l") 
		}

		pp <- ggplotly(p)
		#ggsave(filename=tf1, plot=pp) # DOesn't really work well... using htmlwidgets instead.
		htmlwidgets::saveWidget(widget=pp, file=snr_filename, selfcontained=FALSE)
		
		# Write a PNG for comparision
		png(file=snr_filename_png, width=png_pixel_width, height=png_pixel_height, units="px")
		print(p)
		dev.off()
	}
}

facet_categoryZ <- "ById"
facet_wrap_column_nameZ <- "IdDescriptive"
subset_column_nameeZ <- "snr_category"
snr_chart_categoryZ = "Category1"
snr_set_constraintsZ = "Boilerplate text here"
snr_miss_conditionZ = "...more boilerplate text here..."

ggplot_by_vehicle(p_facet_category = facet_categoryZ
					  ,p_facet_wrap_column_name = facet_wrap_column_nameZ
					  ,p_subset_column_name = subset_column_nameeZ
					  ,p_snr_chart_category = snr_chart_categoryZ
					  ,p_snr_set_constraints = snr_set_constraintsZ
					  ,p_snr_miss_condition = snr_miss_conditionZ)

					  
facet_categoryZ <- "ByCategory"
facet_wrap_column_nameZ <- "snr_category"
subset_column_nameeZ <- "Id"
snr_chart_categoryZ = "11514"
snr_set_constraintsZ = "Boilerplate text here"
snr_miss_conditionZ = "...more boilerplate text here..."

ggplot_by_vehicle(p_facet_category = facet_categoryZ
					  ,p_facet_wrap_column_name = facet_wrap_column_nameZ
					  ,p_subset_column_name = subset_column_nameeZ
					  ,p_snr_chart_category = snr_chart_categoryZ
					  ,p_snr_set_constraints = snr_set_constraintsZ
					  ,p_snr_miss_condition = snr_miss_conditionZ)
