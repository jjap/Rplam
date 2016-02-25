# Fonctions utiles en R
#

# Determiner le volume à l'aide de la formule de Smalian

smalian3d.scalar <- function (gb, fb, lenght, d1m){
                    # diam et longueur en cm, resultat en m3
                    if(missing(d1m)) {
                      # 2 diamètres
                      (gb^2 + fb^2) * lenght * pi / 8000000
                    } else {
                      # 3 diamètres
                      if (is.na(d1m)){
                        (gb^2 + fb^2) * lenght * pi / 8000000 }
                      else {
                        ((gb^2 + d1m^2) * 100 + (d1m^2 + fb^2) * (lenght - 100)) * pi / 8000000
                      }
                    }
             }
             
smalian3d <- Vectorize(smalian3d.scalar)

# Arrondir et formarter de façon intelligente 
# From r/broman

myround <-
    function(x, digits=1)
{
    if(digits < 1)
        stop("This is intended for the case digits >= 1.")

    if(length(digits) > 1) {
        digits <- digits[1]
        warning("Using only digits[1]")
    }

    tmp <- sprintf(paste("%.", digits, "f", sep=""), x)

    # deal with "-0.00" case
    zero <- paste0("0.", paste(rep("0", digits), collapse=""))
    tmp[tmp == paste0("-", zero)] <- zero

    tmp
}
