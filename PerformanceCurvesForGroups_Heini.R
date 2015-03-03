

##### Printing out performance curves to different groups ####
# For this code you need the output curve files and a table that has the features in 
# _exactly_ same order as in your .spp file, followed by respective groupings in following columns.

# Make sure your dir is set to where all your needed input files are.
# Put all curve files into a folder called 'Output', keep rest in the dir folder.


files <- list.files('Outputs/')[14:25]

for (h in 1:length(files)){

	# Name of original curves file (THIS NEEDS TO BE UPDATED)
	input <- paste('Outputs/', files[h], sep='')
	
	# Upload the curves file
	curves <- read.table(input, skip=1, header=F, sep='')
	
	# Upload groupings file that shows to which groups each of the species belongs to
	groups <- read.table('Groups.txt', header=T, sep='\t')
	n.groupings <- 0
	for (i in 2:ncol(groups)){
		n.groupings <- n.groupings + max(groups[i])
	}
	
	# Create an output table
	group.curves <- data.frame(matrix(NA, nrow(curves), (4+3*n.groupings)))
	dim(group.curves)
	header <- c()
	for (i in 2:ncol(groups)){
		for (k in 1:max(groups[i])){
			header <- c(header, paste(colnames(groups)[i], k, '_min', sep=''))
			header <- c(header, paste(colnames(groups)[i], k, '_mean', sep=''))
			header <- c(header, paste(colnames(groups)[i], k, '_max', sep=''))
		}
	}
	header
	colnames(group.curves) <- c('Prop_landscape_lost', 'cost_needed_for_top_fraction', 'ave_prop_rem', 'min_prop_rem', header)
	colnames(group.curves)
	
	# Calculate values to the table
	group.curves[,1] <- curves[,1]
	group.curves[,2] <- curves[,2]
	group.curves[,3] <- curves[,4]
	group.curves[,4] <- curves[,3]
	
	sp.curves <- curves[,-c(1:7)]
	dim(sp.curves)
	
	round <- 0
	for (j in 2:ncol(groups)){
		for (k in 1:max(groups[j])){
			for (i in 1:nrow(sp.curves)){
				group.curves[i,5+(3*round)] <- min(as.numeric(sp.curves[i, groups[,j] == k]))
				group.curves[i,6+(3*round)] <- mean(as.numeric(sp.curves[i, groups[,j] == k]))
				group.curves[i,7+(3*round)] <- max(as.numeric(sp.curves[i, groups[,j] == k]))
			}
			round <- round + 1
		}
	}
	head(group.curves, 30)
	
	# Write the output table
	write.table(group.curves, paste('Group.curves_', files[h], sep=''), col.names=T, row.names=F)
	
}





### DRAWING PLOTS


# Upload files

optimal <- read.table('Group.curves_scenario01_unconstrained.curves.txt', header=T, sep=' ')
sce2 <- read.table('Group.curves_scenario02_eia1.curves.txt', header=T, sep=' ')
sce3 <- read.table('Group.curves_scenario03_eia2.curves.txt', header=T, sep=' ')
sce4 <- read.table('Group.curves_scenario04_eia3_Option1.curves.txt', header=T, sep=' ')
sce5 <- read.table('Group.curves_scenario05_eia3_Option2.curves.txt', header=T, sep=' ')
sce6 <- read.table('Group.curves_scenario06_brm_eil4.curves.txt', header=T, sep=' ')
sce7 <- read.table('Group.curves_scenario07_rural_res.curves.txt', header=T, sep=' ')
sce8 <- read.table('Group.curves_scenario08_infrastructure.curves.txt', header=T, sep=' ')
sce9 <- read.table('Group.curves_scenario09_pines.curves.txt', header=T, sep=' ')
sce10 <- read.table('Group.curves_scenario10_ProtectedAreas.curves.txt', header=T, sep=' ')
sce11 <- read.table('Group.curves_scenario11_AllOption1.curves.txt', header=T, sep=' ')
sce12 <- read.table('Group.curves_scenario12_AllOption2.curves.txt', header=T, sep=' ')

colnames(optimal)


png('Fig_curves_EIAs.png', width = 970, height = 1200)
set.panel(2,1)
par(cex = 2)
par(mar = c(0, 0.5, 0.5, 8.3), oma = c(4, 4, 0.5, 0.5), xpd=TRUE) #‘c(bottom, left, top, right)’
par(mgp = c(2, 0.6, 0))
par(bty = "l")
linew <- 3

col.a <- c('dark grey', 'skyblue2', 'skyblue4', 'darkolivegreen3', 'darkolivegreen4')
col.b <- c('lightskyblue4', 'skyblue2', 'steelblue', 'dodgerblue3', 'dodgerblue4')
col.c <- c('rosybrown4', 'lightcoral', 'indianred', 'brown1', 'red4')



# mean and min across all
plot(optimal$Prop_landscape_lost, optimal$ave_prop_rem, col=col.a[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$min_prop_rem, col=col.a[1], lty=3, lwd = linew)
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce2$Prop_landscape_lost, sce2$ave_prop_rem, col=col.a[2], lwd = linew)
	lines(sce2$Prop_landscape_lost, sce2$min_prop_rem, col=col.a[2], lty=3, lwd = linew) 
	lines(sce3$Prop_landscape_lost, sce3$ave_prop_rem, col=col.a[3], lwd = linew)
	lines(sce3$Prop_landscape_lost, sce3$min_prop_rem, col=col.a[3], lty=3, lwd = linew) 
	lines(sce4$Prop_landscape_lost, sce4$ave_prop_rem, col=col.a[4], lwd = linew)
	lines(sce4$Prop_landscape_lost, sce4$min_prop_rem, col=col.a[4], lty=3, lwd = linew) 
	lines(sce5$Prop_landscape_lost, sce5$ave_prop_rem, col=col.a[5], lwd = linew)
	lines(sce5$Prop_landscape_lost, sce5$min_prop_rem, col=col.a[5], lty=3, lwd = linew) 

leg.text <- c('Unconstrained', 'EIA1', 'EIA2', 'EIA3 (Opt1)', 'EIA3 (Opt2)')
legend('topright', inset = c(-0.37,0), leg.text, lty=1, title = expression(bold('All species')), title.adj = 0, lwd = linew+2,
   col=col.a, bty='n', cex=0.8, adj = 0, y.intersp =1.15)


# mean and min across MNES and non-MNES
# non-MNES
plot(optimal$Prop_landscape_lost, optimal$MNES_group2_mean, col=col.b[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group2_min, col=col.b[1], lty=3, lwd = linew) 
	axis(1, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2))
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce2$Prop_landscape_lost, sce2$MNES_group2_mean, col=col.b[2], lwd = linew)
	lines(sce2$Prop_landscape_lost, sce2$MNES_group2_min, col=col.b[2], lty=3, lwd = linew) 
	lines(sce3$Prop_landscape_lost, sce3$MNES_group2_mean, col=col.b[3], lwd = linew)
	lines(sce3$Prop_landscape_lost, sce3$MNES_group2_min, col=col.b[3], lty=3, lwd = linew) 
	lines(sce4$Prop_landscape_lost, sce4$MNES_group2_mean, col=col.b[4], lwd = linew)
	lines(sce4$Prop_landscape_lost, sce4$MNES_group2_min, col=col.b[4], lty=3, lwd = linew) 
	lines(sce5$Prop_landscape_lost, sce5$MNES_group2_mean, col=col.b[5], lwd = linew)
	lines(sce5$Prop_landscape_lost, sce5$MNES_group2_min, col=col.b[5], lty=3, lwd = linew) 

# MNES
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_mean, col=col.c[1], type='l', lwd = 2)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_min, col=col.c[1], lty=3, lwd = 2) 

	lines(sce2$Prop_landscape_lost, sce2$MNES_group1_mean, col=col.c[2], lwd = linew)
	lines(sce2$Prop_landscape_lost, sce2$MNES_group1_min, col=col.c[2], lty=3, lwd = linew) 
	lines(sce3$Prop_landscape_lost, sce3$MNES_group1_mean, col=col.c[3], lwd = linew)
	lines(sce3$Prop_landscape_lost, sce3$MNES_group1_min, col=col.c[3], lty=3, lwd = linew) 
	lines(sce4$Prop_landscape_lost, sce4$MNES_group1_mean, col=col.c[4], lwd = linew)
	lines(sce4$Prop_landscape_lost, sce4$MNES_group1_min, col=col.c[4], lty=3, lwd = linew) 
	lines(sce5$Prop_landscape_lost, sce5$MNES_group1_mean, col=col.c[5], lwd = linew)
	lines(sce5$Prop_landscape_lost, sce5$MNES_group1_min, col=col.c[5], lty=3, lwd = linew) 

legend('topright', inset = c(-0.37,0), leg.text, title = expression(bold('non-MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.b, bty='n', cex=0.8, adj = c(0,0), y.intersp =1.15)
legend('topright', inset = c(-0.37,0.4), leg.text, title = expression(bold('MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.c, bty='n', cex=0.8, adj = c(0,0), y.intersp =1.15)

mtext('Proportion of landscape removed', side = 1, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2, adj = 0.25)
mtext('Proportion of feature distributions remaining', side = 2, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2)

dev.off()




# Development components plot

png('Fig_curves_DevComp.png', width = 970, height = 1200)
set.panel(2,1)
par(cex = 2)
par(mar = c(0, 0.5, 0.5, 8.3), oma = c(4, 4, 0.5, 0.5), xpd=TRUE) #‘c(bottom, left, top, right)’
par(mgp = c(2, 0.6, 0))
par(bty = "l")
linew <- 3

# mean and min across all
plot(optimal$Prop_landscape_lost, optimal$ave_prop_rem, col=col.a[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$min_prop_rem, col=col.a[1], lty=3, lwd = linew)
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce6$Prop_landscape_lost, sce6$ave_prop_rem, col=col.a[2], lwd = linew)
	lines(sce6$Prop_landscape_lost, sce6$min_prop_rem, col=col.a[2], lty=3, lwd = linew) 
	lines(sce7$Prop_landscape_lost, sce7$ave_prop_rem, col=col.a[3], lwd = linew)
	lines(sce7$Prop_landscape_lost, sce7$min_prop_rem, col=col.a[3], lty=3, lwd = linew) 
	lines(sce8$Prop_landscape_lost, sce8$ave_prop_rem, col=col.a[4], lwd = linew)
	lines(sce8$Prop_landscape_lost, sce8$min_prop_rem, col=col.a[4], lty=3, lwd = linew) 
	lines(sce9$Prop_landscape_lost, sce9$ave_prop_rem, col=col.a[5], lwd = linew)
	lines(sce9$Prop_landscape_lost, sce9$min_prop_rem, col=col.a[5], lty=3, lwd = linew) 

leg.text <- c('Unconstrained', 'BRM (EIL4)', 'Rural', 'Infra', 'Pines')
legend('topright', inset = c(-0.37,0), leg.text, lty=1, title = expression(bold('All species')), title.adj = 0, lwd = linew+2,
   col=col.a, bty='n', cex=.8, adj = 0, y.intersp =1.15)


# mean and min across MNES and non-MNES
# non-MNES
plot(optimal$Prop_landscape_lost, optimal$MNES_group2_mean, col=col.b[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group2_min, col=col.b[1], lty=3, lwd = linew) 
	axis(1, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2))
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce6$Prop_landscape_lost, sce6$MNES_group2_mean, col=col.b[2], lwd = linew)
	lines(sce6$Prop_landscape_lost, sce6$MNES_group2_min, col=col.b[2], lty=3, lwd = linew) 
	lines(sce7$Prop_landscape_lost, sce7$MNES_group2_mean, col=col.b[3], lwd = linew)
	lines(sce7$Prop_landscape_lost, sce7$MNES_group2_min, col=col.b[3], lty=3, lwd = linew) 
	lines(sce8$Prop_landscape_lost, sce8$MNES_group2_mean, col=col.b[4], lwd = linew)
	lines(sce8$Prop_landscape_lost, sce8$MNES_group2_min, col=col.b[4], lty=3, lwd = linew) 
	lines(sce9$Prop_landscape_lost, sce9$MNES_group2_mean, col=col.b[5], lwd = linew)
	lines(sce9$Prop_landscape_lost, sce9$MNES_group2_min, col=col.b[5], lty=3, lwd = linew) 

# MNES
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_mean, col=col.c[1], type='l', lwd = 2)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_min, col=col.c[1], lty=3, lwd = 2) 

	lines(sce6$Prop_landscape_lost, sce6$MNES_group1_mean, col=col.c[2], lwd = linew)
	lines(sce6$Prop_landscape_lost, sce6$MNES_group1_min, col=col.c[2], lty=3, lwd = linew) 
	lines(sce7$Prop_landscape_lost, sce7$MNES_group1_mean, col=col.c[3], lwd = linew)
	lines(sce7$Prop_landscape_lost, sce7$MNES_group1_min, col=col.c[3], lty=3, lwd = linew) 
	lines(sce8$Prop_landscape_lost, sce8$MNES_group1_mean, col=col.c[4], lwd = linew)
	lines(sce8$Prop_landscape_lost, sce8$MNES_group1_min, col=col.c[4], lty=3, lwd = linew) 
	lines(sce9$Prop_landscape_lost, sce9$MNES_group1_mean, col=col.c[5], lwd = linew)
	lines(sce9$Prop_landscape_lost, sce9$MNES_group1_min, col=col.c[5], lty=3, lwd = linew) 

legend('topright', inset = c(-0.37,0), leg.text, title = expression(bold('non-MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.b, bty='n', cex=.8, adj = c(0,0), y.intersp =1.15)
legend('topright', inset = c(-0.37,0.4), leg.text, title = expression(bold('MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.c, bty='n', cex=.8, adj = c(0,0), y.intersp =1.15)

mtext('Proportion of landscape removed', side = 1, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2, adj = 0.25)
mtext('Proportion of feature distributions remaining', side = 2, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2)

dev.off()



# AllOptions plot

col.a <- c('dark grey', 'darkolivegreen3', 'darkolivegreen4')
col.b <- c('lightskyblue4', 'skyblue2', 'dodgerblue3')
col.c <- c('rosybrown4', 'lightcoral', 'brown1')

png('Fig_curves_AllOption.png', width = 970, height = 1200)
set.panel(2,1)
par(cex = 2)
par(mar = c(0, 0.5, 0.5, 8.3), oma = c(4, 4, 0.5, 0.5), xpd=TRUE) #‘c(bottom, left, top, right)’
par(mgp = c(2, 0.6, 0))
par(bty = "l")
linew <- 3

# mean and min across all
plot(optimal$Prop_landscape_lost, optimal$ave_prop_rem, col=col.a[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$min_prop_rem, col=col.a[1], lty=3, lwd = linew)
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce11$Prop_landscape_lost, sce11$ave_prop_rem, col=col.a[2], lwd = linew)
	lines(sce11$Prop_landscape_lost, sce11$min_prop_rem, col=col.a[2], lty=3, lwd = linew) 
	lines(sce12$Prop_landscape_lost, sce12$ave_prop_rem, col=col.a[3], lwd = linew)
	lines(sce12$Prop_landscape_lost, sce12$min_prop_rem, col=col.a[3], lty=3, lwd = linew) 

leg.text <- c('Unconstrained', 'All (Opt1)', 'All (Opt2)')
legend('topright', inset = c(-0.37,0), leg.text, lty=1, title = expression(bold('All species')), title.adj = 0, lwd = linew+2,
   col=col.a, bty='n', cex=.8, adj = 0, y.intersp =1.15)


# mean and min across MNES and non-MNES
# non-MNES
plot(optimal$Prop_landscape_lost, optimal$MNES_group2_mean, col=col.b[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group2_min, col=col.b[1], lty=3, lwd = linew) 
	axis(1, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2))
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce11$Prop_landscape_lost, sce11$MNES_group2_mean, col=col.b[2], lwd = linew)
	lines(sce11$Prop_landscape_lost, sce11$MNES_group2_min, col=col.b[2], lty=3, lwd = linew)  
	lines(sce12$Prop_landscape_lost, sce12$MNES_group2_mean, col=col.b[3], lwd = linew)
	lines(sce12$Prop_landscape_lost, sce12$MNES_group2_min, col=col.b[3], lty=3, lwd = linew) 


# MNES
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_mean, col=col.c[1], type='l', lwd = linew)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_min, col=col.c[1], lty=3, lwd = linew) 

	lines(sce11$Prop_landscape_lost, sce11$MNES_group1_mean, col=col.c[2], lwd = linew)
	lines(sce11$Prop_landscape_lost, sce11$MNES_group1_min, col=col.c[2], lty=3, lwd = linew) 
	lines(sce12$Prop_landscape_lost, sce12$MNES_group1_mean, col=col.c[3], lwd = linew)
	lines(sce12$Prop_landscape_lost, sce12$MNES_group1_min, col=col.c[3], lty=3, lwd = linew) 

legend('topright', inset = c(-0.37,0), leg.text, title = expression(bold('non-MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.b, bty='n', cex=.8, adj = c(0,0), y.intersp =1.15)
legend('topright', inset = c(-0.37,0.3), leg.text, title = expression(bold('MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.c, bty='n', cex=.8, adj = c(0,0), y.intersp =1.15)

mtext('Proportion of landscape removed', side = 1, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2, adj = 0.25)
mtext('Proportion of feature distributions remaining', side = 2, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2)

dev.off()




# PAs plot

library(colorRamps)
library(fields)
col.a <- c('dark grey', 'darkolivegreen3')
col.b <- c('lightskyblue4', 'skyblue2')
col.c <- c('rosybrown4', 'lightcoral')


png('Fig_curves_PAs.png', width = 900, height = 1200)
set.panel(2,1)
par(cex = 2)
par(mar = c(0, 0.5, 0.5, 8.3), oma = c(4, 4, 3.5, 0.5), xpd=TRUE) #‘c(bottom, left, top, right)’
par(mgp = c(2, 0.6, 0))
par(bty = "l")
linew <- 3

# mean and min across all
plot(optimal$Prop_landscape_lost, optimal$ave_prop_rem, col=col.a[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$min_prop_rem, col=col.a[1], lty=3, lwd = linew)
#	axis(1, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2))
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce10$Prop_landscape_lost, sce10$ave_prop_rem, col=col.a[2], lwd = linew)
	lines(sce10$Prop_landscape_lost, sce10$min_prop_rem, col=col.a[2], lty=3, lwd = linew) 

leg.text <- c('Unconstrained', 'Current reserves')
legend('topright', inset = c(-0.43,0), leg.text, lty=1, title = expression(bold('All species')), title.adj = 0, lwd = linew+2,
   col=col.a, bty='n', cex=.8, adj = 0, y.intersp =1.15)

# mean and min across MNES and non-MNES
# non-MNES
plot(optimal$Prop_landscape_lost, optimal$MNES_group2_mean, col=col.b[1], type='l', lwd = linew, axes=F)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group2_min, col=col.b[1], lty=3, lwd = linew) 
	axis(1, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2))
	axis(2, col = "grey40", col.axis = "grey20", at = seq(0, 1, 0.2)) 

	lines(sce10$Prop_landscape_lost, sce10$MNES_group2_mean, col=col.b[2], lwd = linew)
	lines(sce10$Prop_landscape_lost, sce10$MNES_group2_min, col=col.b[2], lty=3, lwd = linew)  

# MNES
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_mean, col=col.c[1], type='l', lwd = linew)
	lines(optimal$Prop_landscape_lost, optimal$MNES_group1_min, col=col.c[1], lty=3, lwd = linew) 

	lines(sce10$Prop_landscape_lost, sce10$MNES_group1_mean, col=col.c[2], lwd = linew)
	lines(sce10$Prop_landscape_lost, sce10$MNES_group1_min, col=col.c[2], lty=3, lwd = linew) 

legend('topright', inset = c(-0.43,0), leg.text, title = expression(bold('non-MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.b, bty='n', cex=.8, adj = c(0,0), y.intersp =1.15)
legend('topright', inset = c(-0.43,0.23), leg.text, title = expression(bold('MNES species')), title.adj = 0, lty=1, lwd = linew+2,
   col=col.c, bty='n', cex=.8, adj = c(0,0), y.intersp =1.15)

mtext('Proportion of landscape removed', side = 1, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2, adj = 0.2)
mtext('Proportion of feature distributions remaining', side = 2, outer = TRUE, cex = 2, line = 2.2, col = "grey20", font=2)


par(oma=c(37,4.6,0,7), bty='o')# reset margin to be much smaller.
classes <- c(0,0.337,0.408,0.827,1)
image.plot(add=F, legend.only=TRUE, zlim=c(0,1), breaks=classes, smallplot= c(0.016,0.9,0.75,0.98), #‘c(bottom, left, top, right)’
	col=c('white', 'darkolivegreen1', 'yellowgreen', 'darkolivegreen4'), axis.args=list(at=c(0.17,0.375,0.62,0.915), tick=F, line=-1,
	labels=c('Unprotected', 'Other', 'IUCN VI', 'IUCN I-IV'), cex.axis=0.7), horizontal=T)

dev.off()




### Number of cells in each PA category
# IUCN1_4 <- 99209
# IUCN6 <- 240490
# OtherPA <- 41004
# nonPA <- 193498



### Colors

col.wheel <- function (str, nearby = 3, cex = 0.75) {
    cols <- colors()
    hsvs <- rgb2hsv(col2rgb(cols))
    srt <- order(hsvs[1, ], hsvs[2, ], hsvs[3, ])
    cols <- cols[srt]
    ind <- grep(str, cols)
    if (length(ind) < 1) 
        stop("no colour matches found", call. = FALSE)
    s.ind <- ind
    if (nearby > 1) 
        for (i in 1:nearby) {
            s.ind <- c(s.ind, ind + i, ind - i)
        }
    ind <- sort(unique(s.ind))
    ind <- ind[ind <= length(cols)]
    cols <- cols[ind]
    pie(rep(1, length(cols)), labels = cols, col = cols, cex = cex)
    cols
}

col.wheel('green')


