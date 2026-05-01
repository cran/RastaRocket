.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome dear user, check vignette('SetupProject') and vignette('RastaRocketVignette') for some help. May the force be with you and make your analysis swift and smooth.")
  vec_citation <- c("On est exhaustifs dans la bouse ! 04/04/2023",
                    "Toute pand\u00e9mie est une synd\u00e9mie en fait non ? 11/09/2023",
                    "Pas besoin de creuser, la merde est d\u00e9j\u00e0 l\u00e0 ! 22/03/2023",
                    "Je vais encore remettre une pi\u00E8ce dans la machine \u00E0 tout changer. 30/04/2026")
  packageStartupMessage("--- Now a small citation from great philosophers ---")
  packageStartupMessage(sample(vec_citation, size = 1))
}