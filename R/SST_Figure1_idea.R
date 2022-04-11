# goals:
# 1. boundaries of our area demarcations (definitely)
# 2. locations of individual sample locations used in this analysis
# 3. latitude and depth distribution of the small sizes that were the focus of this paper
# 4. latitude and depth distribution of different size classes illustrating apparent ontogenetic movement

mydir <- "c:/SS/Thornyheads/SST_migration/"
require(maps)
require(mapdata)

# load info on samples that were used in the analysis
if (!exists("samps.all")) {
  samps.all <- read.csv(file.path(
    mydir,
    "data/selected shortspine samples for migration study.csv"
  ))
  samps.used <- read.csv(file.path(
    mydir,
    "data/SST_LA_TE_AllDataUpdated092016.csv"
  ))

  # files shared by Emmannis in August 2019
  # first file has repeat observations at different locations along otolith
  # and 100 unique barcodes
  dat1 <- read.csv(file.path(mydir, "data/sst.pro.te.sm_082618.csv"))
  # second file has 96 observations all with unique barcodes
  dat2 <- read.csv(file.path(mydir, "data/sst.te.si.set.sm_082619.csv"))
  # get 100 unique barcodes from larger file
  samps.used.small <- dat1[!duplicated(dat1$Barcode), ]

  samps <- merge(samps.used.small, samps.all, by.x = "Barcode", by.y = "AGE_BARCODE")
}

# extract survey data from online data warehouse if not already loaded
if (!exists("catch.WCGBTS")) {
  if (FALSE) {
    # install package to extract survey data
    remotes::install_github("nwfsc-assess/nwfscSurvey", build_vignettes = TRUE)
    catch.WCGBTS <- nwfscSurvey::PullCatch.fn(
      Name = "shortspine thornyhead",
      SurveyName = "NWFSC.Combo"
    )
    bio.WCGBTS <- nwfscSurvey::PullBio.fn(
      Name = "shortspine thornyhead",
      SurveyName = "NWFSC.Combo"
    )
    save(catch.WCGBTS, bio.WCGBTS, file = file.path(mydir, "data/survey_data.Rdata"))
  }
  # load file created by commands above
  load(file.path(mydir, "data/survey_data.Rdata"))
}
# process survey data to get proportion of small individuals
if (!exists("catch.small")) {
  catch.small <- catch.WCGBTS[catch.WCGBTS$Year %in% 2011:2013, ]
  bio.small <- bio.WCGBTS[bio.WCGBTS$Year %in% 2011:2013, ]
  catch.small$frac6to17 <- NA
  for (irow in 1:nrow(catch.small)) {
    if (irow %% 100 == 0) {
      print(irow)
    }
    lens <- bio.small$Length_cm[bio.small$Trawl_id == catch.small$Trawl_id[irow]]
    catch.small$frac6to17[irow] <- mean(round(lens) %in% 6:17)
  }
}

if (FALSE) {
  # skip processing of bathymetry data
  # (saved as .Rdata file at end of this section)
  # ETOPO1 bedrock grid data http://maps.ngdc.noaa.gov/viewers/wcs-client/
  d <- read.table(file.path(mydir, "etopo1_bedrock.xyz"), header = FALSE)
  ## > head(d)
  ##          V1 V2   V3
  ## 1 -128.0000 50 -860
  ## 2 -127.9833 50 -865
  ## 3 -127.9667 50 -884
  ## 4 -127.9500 50 -902
  ## 5 -127.9333 50 -899
  ## 6 -127.9167 50 -861
  names(d) <- c("lon", "lat", "depth")

  # get vectors of lat/lon
  ## lonvec = seq(-128,-114,1/60) #1-minute grid
  ## latvec = seq(30,50,1/60)
  lonvec <- sort(unique(d$lon))
  latvec <- sort(unique(d$lat))
  nlon <- length(lonvec)
  nlat <- length(latvec)
  # convert depth values to a matrix
  d2 <- matrix(NA, nrow = nlat, ncol = nlon)
  for (ilat in 1:nlat) {
    lat <- latvec[ilat]
    # subset big data.frame for matching latitude
    d.sub <- d[d$lat == lat, ]
    for (ilon in 1:nlon) {
      lon <- lonvec[ilon]
      # assign depth from row which also matches longitude
      d2[ilat, ilon] <- d.sub$depth[d.sub$lon == lon]
    }
    print(ilat)
  }

  # save processed bathymetry data as .Rdata file
  save(d2, lonvec, latvec, file = file.path(mydir, "data/depth_data_SST_sample_map.Rdata"))
}

# load bathymetry data
if (!exists("d2")) {
  load(file.path(mydir, "data/depth_data_SST_sample_map.Rdata"))
}

# new and fancier, extending to full range of topo.colors separately for land and water
dmin <- min(d2)
dmax <- max(d2)
NwaterColors <- round(abs(dmin))
NlandColors <- round(abs(dmax))

# taking first 1/3 of topo.colors of length NwaterColors for water
waterColorVec <- topo.colors(NwaterColors * 3)[1:NwaterColors]
# taking second 2/3 of topo.colors of length NlandColors for land
# landColorVec <- topo.colors(NlandColors*3/2)[round(NlandColors/2+1):round(1.5*NlandColors)]
# landColorVec <- rep('grey60', length=NlandColors)
landColorVec <- rep(tail(waterColorVec, 1), NlandColors) # repeating shallowest water color

# combining
allColorVec <- c(waterColorVec, landColorVec)


# make PNG file
png(file.path(mydir, "figs/SST_Figure1_11April2022.png"),
  width = 9, height = 10, units = "in", res = 300
)
par(mar = c(0, 0.5, 1.4, 1.4), oma = c(4, 4, 0, 0), mfrow = c(1, 2))

for (iplot in 1:2) {
  message("starting panel ", iplot, " of 2")
  if (iplot == 1) {
    plot(0,
      type = "n", xlim = c(650, 40), ylim = c(32, 49), xaxs = "i", yaxs = "i",
      axes = FALSE, xlab = "", ylab = ""
    )
    axis(1, at = c(seq(600, 100, by = -100), 55))
    axis(1, at = 55)
  }
  if (iplot == 2) {
    plot(0,
      type = "n", xlim = c(-126, -117), ylim = c(32, 49), xaxs = "i", yaxs = "i",
      axes = FALSE, xlab = "", ylab = ""
    )
  }

  # add shades of blue indicating depth
  image(lonvec, latvec, t(as.matrix(d2)), col = allColorVec, add = TRUE)
  # add contours at 1000m, 500m, and 100m
  contourColorVec <- c("grey30", "grey40", "grey60", "grey50")
  contour(
    x = lonvec, y = latvec, z = t(d2), levels = c(-1000, -500, -100),
    drawlabels = FALSE, add = TRUE, lwd = 1, col = contourColorVec
  )

  # add map
  if (iplot == 2) {
    map("worldHires",
      xlim = c(-126, -117), ylim = c(32, 49), xaxs = "i", yaxs = "i",
      fill = TRUE, col = "grey", add = TRUE
    )
    axis(1, seq(-126, -117, 2), seq(126, 117, -2))
  }

  # add axes for first panel
  axis2.vec <- c(32, 36, 40, 44, 46.25, 47, 49)
  axis2.text <- c("32", "36", "40", "44", "46.25", "47", "49")
  if (iplot == 1) {
    axis(2, at = axis2.vec, label = axis2.text, las = 1)
    # axis(1, seq(-126,-117,2),paste(seq(126,117,-2),'<U+FFFD>W', sep=''))
    # axis(2, seq(32,49,2),paste(seq(32,49,2),'<U+FFFD>N', sep=''), las=1)
    abline(v = 500, lty = 3)
    # add region labels
    par(mgp = c(3, 0.25, 0))
    axis(4,
      at = c(48, 45.125, 42, 38, 34),
      label = c("1-N", "2-N", "3-N", "4-S", "5-S"),
      las = 1,
      tick = FALSE
    )
    par(mgp = c(3, 1, 0))
  }
  abline(h = axis2.vec, lty = 3)

  # add labels on depth contours
  if (iplot == 2) {
    text(-124.1, 47.3,
      labels = "100m", cex = 0.7,
      col = contourColorVec[4], pos = 4
    )
    arrows(-123.9, 47.3, -124.65, 47.1,
      length = 0.05,
      col = contourColorVec[4], lty = 1
    )
    text(-125.1, 46.8,
      labels = "500m", cex = 0.7,
      col = contourColorVec[2], pos = 4
    )
    text(-124.7, 46.5,
      labels = "1000m", cex = 0.7,
      col = contourColorVec[1], pos = 2
    )
  }
  # colors for each year
  yr.col.vec <- c(
    rgb(.8, 0, .5, 1),
    rgb(1, .4, 0, 1),
    rgb(1, .8, 0, 1)
  )
  yr.col.vec2 <- c(
    rgb(.8, 0, .5, 1, maxColorValue = 1.5),
    rgb(1, .4, 0, 1, maxColorValue = 1.5),
    rgb(1, .8, 0, 1, maxColorValue = 1.5)
  )
  samps$yr.col <- yr.col.vec[as.numeric(as.factor(samps$Year))]
  samps$yr.col2 <- yr.col.vec2[as.numeric(as.factor(samps$Year))]

  if (iplot == 1) {
    message("adding gray points")
    # add points where each haul is located in depth vs. latitude panel
    points(
      x = catch.small$Depth_m, y = catch.small$Latitude_dd,
      pch = 4, cex = 0.5, col = gray(0, alpha = 0.3)
    )
    message("adding density points")
    # add points for density of small individuals
    points(
      x = catch.small$Depth_m, y = catch.small$Latitude_dd,
      bg = rgb(0, 1, 0, .3), col = rgb(0, 1, 0, .3), pch = 21,
      cex = .4 * sqrt(catch.small$frac6to17 *
        catch.small$total_catch_numbers /
        catch.small$Area_Swept_ha)
    )
    points(
      x = samps$DEPTH_M, y = samps$HAUL_LATITUDE_DD,
      bg = samps$yr.col, col = samps$yr.col2, pch = 21, cex = .7,
    )
  }
  if (iplot == 2) {
    # add points where each haul is located in map
    points(
      x = samps$HAUL_LONGITUDE_DD, y = samps$HAUL_LATITUDE_DD,
      bg = samps$yr.col, col = samps$yr.col2, pch = 21, cex = .7,
    )
  }

  # axis labels
  if (iplot == 1) {
    mtext(side = 2, line = 2.5, "Latitude (\u00B0N)")
    mtext(side = 4, line = 0.4, "Area")
    mtext(side = 1, line = 2.5, "Depth (m)", outer = FALSE)
  }
  if (iplot == 2) {
    mtext(side = 1, line = 2.5, "Longitude (\u00B0W)", outer = FALSE)
  }

  if (iplot == 2) {
    # box to contain legends
    rect(-121, 39.7, -100, 50, col = "white", border = "black")

    # legend for colors for each year
    legend.text <- c("2011", "2012", "2013")
    legend(-120.7, 48.5,
      title = "Samples used\nby collection year",
      cex = 1, pch = 21, bty = "n", pt.bg = yr.col.vec, col = yr.col.vec2,
      legend = legend.text
    )

    # legend for haul locations and density
    legend.vec <- c(2, 10, 50, 200)
    legend.text <- c("All hauls", paste0(legend.vec, " / ha"))
    legend.col <- c(gray(0, alpha = 0.3), rep(rgb(0, 1, 0, .3), 4))
    legend(-120.5, 45.5,
      title = "Haul locations\n and density of\nsmall individuals",
      cex = 1, pt.cex = c(0.7, 0.4 * sqrt(legend.vec)), pch = c(4, 21, 21, 21, 21), bty = "n",
      pt.bg = legend.col, col = legend.col,
      legend = legend.text
    )

    # legend for depth scale
    depths <- c(0, 100, 500, 1000, 2000)
    legend(-120.3, 42.5,
      title = "Depth",
      fill = rev(waterColorVec)[depths + 1],
      legend = paste(depths, "m"),
      bty = "n"
    )
  }
  box()
  mtext(text = LETTERS[iplot], cex = 1.5, side = 3, line = 0.1, outer = FALSE, adj = 0)
} # end loop over iplot %in% 1:2

# close PNG file
dev.off()
