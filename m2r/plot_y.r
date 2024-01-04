plot_y<-function(.data){
# PURPOSE: plot all variables in the structure 'data'
whichplots <- 0:1
if (is.element('totransf',names(.data))){
    totransf <- .data$totransf
} else {
    totransf <- rep(FALSE,nrow(.data))}

TT<- nrow(.data)
N<-ncol(.data)
tt <- .data$time
ytoplot <- .data
c(ytoplot[5:],totransf) <- ytoplot(5:},totransf) - ytoplot(1:}-4,totransf); ytoplot(1:4,totransf) <- NaN


if (whichplots[1]){
    for (n in 1:N){
        plot(tt, ytoplot(:,n))
        title(paste0(names(.data)[n], 'Interpreter', 'none'))
    }
}

if (whichplots(2)){
    n1 <- ceil(sqrt(N)); n2 <- n1
    #n1 = 6; n2 = 2;
    scnsize <- get(0,'ScreenSize')
    plot_position <- [0.15*scnsize(3),0.06*scnsize(4),0.7*scnsize(3),1.2*scnsize(4)]
    figure('Position', plot_position)
    for (n in 1:N){
        subplot(n1,n2,n)
        plot(tt, ytoplot(:,n))
        tit <- .data.names{n}; if (totransf(n), tit <- [tit ' (yoy)']; }){
        title(tit, 'Interpreter', 'none', 'FontSize', 8)
        axis tight
    }
}
}