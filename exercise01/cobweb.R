# Attribution:
# Code adapted from https://qualityandinnovation.com/2015/07/25/logistic-growth-s-curves-bifurcations-and-lyapunov-exponents-in-r/

stepf <- function(x, r, K) {
    return (r * x * (1 - x/K));
}

iterf <- function(start, f, N, K) {
    x <- numeric(N);
    x[1] <- start;
    for (i in 2:N) {
        x[i] <- f(x[i-1], r, K);
    }
    return (x);
}

coblines <- function(x, color) {
    N <- length(x);
    for (i in 2:N) {
        lines(x=c(x[i-1], x[i-1]), y=c(x[i-1], x[i]), col=color);
        lines(x=c(x[i-1], x[i]),   y=c(x[i],   x[i]), col=color);
    }
}

makeplot <- function(r, K, N, start, color, xmin, ymin, ymax) {
    # Plot f(x)
    title <- paste("r = ", toString(r), ", x_0 = ", toString(start), sep="");
    curve(
          stepf(x, r, K), xmin, K,
          n=100,
          xlim=c(xmin, K), ylim=c(ymin, ymax),
          lwd=3, col='black',
          main=title, xlab="x_t", ylab="x_t+1"
          );

    # Plot diagonal
    abline(0,1);

    # Plot Poincare section
    coblines(iterf(start, stepf, N, K), color);
    
    # ... and a nice grid
    grid();
}

#rs <- c(0.5, 1.5, 2.5);
#xmins <- c(-1.5, 0, 0);
#ymins <- c(-1.5, 0, 0);
#ymaxs <- c(0.25, 0.5, 0.75);
#
#par(mfrow=c(3,3));
#for (i in 1:3) {
#    r <- rs[i];
#    start_low_p=0+1e-1;
#    start_high_m=(r - 1)/r - 1e-1;
#    start_high_p=(r - 1)/r + 1e-1;
#
#    makeplot(r, 1, 200, start_low_p, 'green', xmins[i], ymins[i], ymaxs[i]);
#    makeplot(r, 1, 200, start_high_m, 'red',  xmins[i], ymins[i], ymaxs[i]);
#    makeplot(r, 1, 200, start_high_p, 'blue', xmins[i], ymins[i], ymaxs[i]);
#}

par(mfrow=c(2,3));
rs <- c(3.5, 3.9);
xmins <- c(0, 0);
ymins <- c(0, 0);
ymaxs <- c(1, 1);

for (i in 1:2) {
    r <- rs[i];
    start_low_p=0+1e-1;
    start_high_m=(r - 1)/r - 1e-1;
    start_high_p=(r - 1)/r + 1e-1;

    makeplot(r, 1, 200, start_low_p, 'green', xmins[i], ymins[i], ymaxs[i]);
    makeplot(r, 1, 200, start_high_m, 'red',  xmins[i], ymins[i], ymaxs[i]);
    makeplot(r, 1, 200, start_high_p, 'blue', xmins[i], ymins[i], ymaxs[i]);
}
