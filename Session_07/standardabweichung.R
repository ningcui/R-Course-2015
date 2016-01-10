stdabw <- function(vec){
    sum = 0
    for (i in 1:length(vec)){
        abw = vec[i]-mean(vec)
        sum = sum + abw**2
    }
    variance = sum/(length(vec))
    return(sqrt(variance))
}