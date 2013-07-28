#' Make wave
#'
#' Generate a vector of numeric data points representing a sine wave
#'
#' @export
#' @param freq The frequency of cycles per unit time (e.g. Hz) of the sine wave
#' @param phase The frequency in degrees of the sine wave
#' @param amp The amplitude of the sine wave
#' @param Nsamples The number of samples to generate, this defaults to the time * sample rate.
#' @param samplerate The number of samples to create per unit of time
#' @param time The number of units of time represented by the generated sine wave
#' @param as.time.series Should the result be returned as a time series ts()
#' @return A numeric vector or a time series representing the specified sine wave
#' @note coh is calculated as coh[, i + (j - 1) * (j - 2)/2] <- Mod(pgram[, i, j])^2/(spec[, i] * spec[, j])
#' Mod(pgram[, i, j])^2 #cross-amplitude values
# '(spec[, i] * spec[, j]) #spectrum densitycal estimates for each series
#' @examples 
#' plot(makewave(2,0,3.889*1.6,180,60))
#' sp <- spectrum(ts.union(makewave(15,30,1,180,60),makewave(15.01,60,1,180,60)),span=c(3),taper=0)
#' with(sp,data.frame(freq=freq,sta.freq=freq/60,phase=round(phase,4),coh=round(coh,4),phase.deg=round(phase/(2*pi)*180,2)))[c(35,45,55),]
#' plot(sp$freq,sp$phase/(2*pi)*180,type="l")
#' plot(sp,plot.type="phase")
#' plot(sp,plot.type="coh")
#' plot(ts.union(makewave(15,30,1,180,60),makewave(15.01,60,1,180,60)))
#' Mod(mvfft(ts.union(makewave(15,30,1,180,60),makewave(15.01,60,1,180,60))))

makewave <- function(freq,phase,amp,Nsamples=time*samplerate,samplerate,time=Nsamples/samplerate,as.time.series=TRUE) {
  time <- Nsamples/samplerate
  phase <- phase*(2*pi)/180
  wavetimes <- seq(0+phase,time*freq*pi*2+phase,length.out=Nsamples)
  #plot(1:samples/samplerate,amp*sin(wavetimes),type="l",xlab="Time")
  if (as.time.series) {res <- ts(amp*sin(wavetimes),deltat=1/samplerate)} else {res <- amp*sin(wavetimes)}
  return(res)
}
NULL

#' Find the number of FFT bins
#' @param Nsample numeric The number of samples in the time series
#' @export
fft.binN <- function(Nsample) {
  return(Nsample %/% 2)
}
NULL

#' Find the size of an FFT bin
#' @param sfreq numeric The number of samples per unit of time, e.g. Hz
#' @param Nsample numeric The number of samples in the time series
#' @export
fft.binSize <- function(sfreq,Nsample) {
  return(sfreq/2 / fft.binN(Nsample))
}
NULL

#' Report the frequencies of the the bins in an FFT
#'
#' @note This code might not work
#' @param sfreq numeric The number of samples per unit of time, e.g. Hz
#' @param Nsample numeric The number of samples in the time series
#' @export
fft.binFreqs <- function(sfreq,Nsample) {
  binID <- 0:fft.binN(Nsample)
  bin.size <- fft.binSize(sfreq,Nsample)
  bin.freq <- bin.size*binID
  return(bin.freq)
}
NULL