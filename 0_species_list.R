
#' Species list
#' Rob van Bemmelen
#' v2 9 December 2025

#' List of 14 species considered:
#' Red-throated Diver, Northern Fulmar, Northern Gannet, 
#' Great Skua, Arctic Skua, Black-legged Kittiwake,
#' Herring Gull, Lesser Black-backed Gull,
#' Great Black-backed gull, Sandwich Tern, Common Tern,
#' Razorbill, Common Guillemot,	Atlantic Puffin. 

#' For the distance analysis, only individuals identified to species level are
#' considered, except for Red-throated Diver and for 'commic terns', for which 
#' also unidentified divers were included.

# euring species list
c_euring <- c(
  20, # Red-thraoted Diver
  220, # Northern Fulmar
  710, # Northern Gannet
  5670, # Arctic Skua
  5690, # Great Skua
  5780, # Little Gull
  5910, # Lesser Black-backed Gull
  5920, # Herring Gull
  6000, # Great Black-backed Gull
  6020, # Black-legged Kittiwake
  6110, # Sandwich Tern
  6169, # Commic Tern
  6340, # Common Guillemot
  6360, # Razorbill
  6540) # Atlantic Puffin

# construct species list
d_sps <- data.frame(
  ID = 1:length(c_euring),
  euring = c_euring,
  name_uk = c(
    "Red-thraoted Diver",
    "Northern Fulmar",
    "Northern Gannet",
    "Arctic Skua",
    "Great Skua",
    "Little Gull",
    "Lesser Black-backed Gull",
    "Herring Gull",
    "Great Black-backed Gull",
    "Black-legged Kittiwake",
    "Sandwich Tern",
    "Commic Tern",
    "Common Guillemot",
    "Razorbill",
    "Atlantic Puffin"
  ),
  name_sctf = c(
    "Gavia stellata",
    "Fulmarus glacialis",
    "Morus bassanus",
    "Stercorarius parasiticus",
    "Stercorarius skua",
    "Hydrocoloeus minutus",
    "Larus fuscus",
    "Larus argentatus",
    "Larus marinus",
    "Rissa tridactyla",
    "Thalasseus sandvicensis",
    "Sterna hirundo/paradisaea",
    "Uria aalge",
    "Alca torda",
    "Fratercula arctica"
  ),
  label = c(
    "Red-throated_Diver",
    "Northern_Fulmar",
    "Northern_Gannet",
    "Arctic_Skua",
    "Great_Skua",
    "Little_Gull",
    "Lesser_Black-backed_Gull",
    "Herring_Gull",
    "Great_Black-backed_Gull",
    "Black-legged_Kittiwake",
    "Sandwich_Tern",
    "commic_tern",
    "Common_Guillemot",
    "Razorbill",
    "Atlantic_Puffin"
  )
)

# export file ------------------------

save(
  d_sps,
  file = file.path(
    dirname(rstudioapi::getSourceEditorContext()$path),
    "data",
    "species_list.rdata"
  )
)
