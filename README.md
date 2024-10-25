# The problem to solve:

Here is my problem: I have three different list of bird species names (we'll call these the "focals" for simplification later on):

 * 8748 species beaks
 * 9995 species traits
 * 9993 species locations

And two different phylogenies (we'll call these the "matches"):

 * One with 9993 species (from Jetz et al 2012)
 * One with 11017 species (from Clements et al 2024)

```{r}
## Loading the data
load("algo_design_data.rda")
## The three species lists and the two trees
names(algo_design_data)
```

I don't want to know about birds, nor about taxonomical disputes nor do I want to spend time on this.
The point here is that I'm lazy and I just want to match the three datasets with both trees.
One easy approach is use the function `match` and simply match the species names with the ones in the trees (you can access them using `$tip.label`).

```{r}
## Matching the beaks with the Jetz tree
match_beaks_jetz <- match(algo_design_data$Species_beaks,
                          algo_design_data$Jetz_tree$tip.label)
## This is hopefully a list of 8748 matches with no NAs:
length(match_beaks_jetz)
any(is.na(match_beaks_jetz))
```

Super nice: everything works!
Let's check next tree:

```{r}
## Matching the beaks with the Clements tree
match_beaks_clements <- match(algo_design_data$Species_beaks,
                              algo_design_data$Clements_tree$tip.label)
## This is hopefully a list of 8748 matches with no NAs:
length(match_beaks_clements)
any(is.na(match_beaks_clements))
```

Oops, it doesn't match.
Let now see how many miss match (how many species are in the beaks dataset but not in the tree):

```{r}
## How many are missmatched?
sum(is.na(match_beaks_clements))
```

We have `r sum(is.na(match_beaks_clements))` miss matches, no way I'm going to go through all of them!
I could send this to Gavin or Chris to sort it out for me but it's very unlikely they'll have time to go through a list of `r sum(is.na(match_beaks_clements))` miss matches for me.
Let's try to reduce it to a minimum and let the computer do most of the work.

# The algorithm

## What do we need?

A list of pairs of names with some comments on how sure we are these binomial names are the same. For example, something like that:


Name in focal | Potential in match? | Flag             | Confidence score
--------------|---------------------|------------------|-----------------
Homo sapiens  | HOmo sapiens        | typo             | 1.00
Pan paniscus  | Homo paniscus       | wrong genus      | 0.99
Homo economicus | Homo sapiens      | wrong species    | 0.99
Accipiter chionogaster | Accipiter collaris | wrong species | 0.5
Accipiter chionogaster | Accipiter erythronemius | wrong species | 0.5

Reasons why it's not in the tree? (from most to least likely)

 1. Because they changed the taxonomy? - very very likely. People always do that!
 2. Because there's a type in the species name? - also unlikely, people double check these.
 3. Because it's a bird that we have data for and is not described in a phylogeny? - unlikely, Clements tree has more species (and supposedly all).

For the first reason, there is two ways the species name has changed:

 1. The genus name has changed.
 2. The species name has changed.

## TODO: flagging the type of missmatch

What are all the types of missmatches?

## TODO: measuring the confidence score

 * For a typo: proportion of mismatched characters?
 * For a wrong taxonomy: proportion of potential matches AND or clade distance?
    * For example for the focal `Homo economicus`, I am pretty sure it was supposed to be `Homo sapiens` because there is only one potential and obvious match.
    * For `Pan sapiens`, I am also pretty sure that it's `Homo sapiens` but slightly less (there is an obvious miss-match in the genus name, but there is potential for a missmatch in the species name too).
    * For `Homo troglodytes`, this is less obvious: it's likely to be `Pan troglodytes` because that's a match one node away but it could be a different genus as well (lots of species are called troglodytes). 

Here is a suggest algorithm divided into two cases that should cover all cases:

## Case 1: genus name changed (e.g focal `Homo sapiens` is actually match `Pan sapiens`)

 1. Separate the genus and species names. Then go to 2.
 2. Take the species name and check if there at least one match in the tree `length(match("sapiens", species_list)) > 0`. If yes, go to 3. else go to 6.
 3. Check if there is just one match or more `length(match) == 1`. If yes, go to 4. else go to 5.
 4. The one matching species is probably the correct one, return the pair of focal species name and suspected matching name.
 5. There is more than one species name matching. Find the species name that is the closest phylogeneticaly to the focal genus and return that return the pair of focal species name and suspected matching name. [INCLUDE TRAVERSAL ALGORITHM HERE]
 6. Return the focal name as ambiguous (needs a human to check).

## Case 2: species name changed (e.g. focal `Homo troglodytes` is actually match `Homo sapiens`).

 1. Separate the genus and species names. Then go to 2.
 2. Take the species name and check if there at least one match in the tree `length(match("troglodytes", species_list)) > 0`. If yes, go to 3. else go to 6.
 3. Check how many matches there are. If there is just one, go to 4. Else go to 5.
 4. If there is only one, match check how far clade wise it is to the match. Return the pairs and flag the distance (e.g. return `Homo troglodytes` and `Pan troglodytes` with a clade distance of 1)
 5. If there is more than one, Find the one that is closest one and return the pairs and flag the distance (like in case 1).
 6. If there is no species name match, check the list of matching genus names in the match list (`length(match("Homo", species_list))`). Then go to 7.
 7. Check if any binomial name with the genus name is already matching in the focal list (e.g. Does `Homo sapiens` already exist in the focal list). If yes, go to 8. Else go to 9.
 8. The binomial name might be a synonym, return it as ambiguous (needs a human to check - e.g. the match list only contains `Homo sapiens` and the focal list contains `Homo sapiens` AND `Homo economicus` are they both synonyms?).
 9. Attribute the species name to focal binomial name to the first match binomial name and flag it to the user (e.g "We changed `Homo troglodytes` by `Homo sapiens` because we know it's at least in that genus.")





# General functions:

-splitting binomials TODO: Joey
-counting characters TODO: Elle
-counting differences in characters TODO: Kat
-counting tree distances TODO: Thomas
-counting number of species per genus: TODO Caleb
-ranking/sorting mismatches: TODO Joey
-detecing the number of species in the same genus that are not matching (de matching function): TODO ROb

-labeling the type of missmatch
-[black box] giving a confidence score

# Tasks

Step
 - Extract the names from the tree (make it the FOCAL list)
Step 
 - Match the names (using match()) remove the ones we don't care
Step
 - Separating Genus and species name but keeping track of the pairs


# TYPO MATCHING
 - [ ] TODO: for Kat
Algorithm for detecting typos:
 1- count number of characters in MATCH
 2- compare to the same n characters in FOCAL
 3- measure the number of different characters
 4- output the one with the least difference
Algorithm for detecting different latin bullshit
    Some kind of matching with wild card: e.g. *us becomes *ae

# MONOTYPIC MATCHING
 - [ ] TODO: for Caleb
Algo for detecting number of species per genus.
    Check the differences in genuses that have just one species
    Easy match of monotypic binomials between MATCH and FOCAL, if there is no match, it's a problem with genus name.
    And then match the species names.

# GENUS OR SPECIES NAME MATCHING
- [ ] TODO: for Thomas
Algo for genus name change
Algo for species name change

# MATCH LEFTOVER MATCHING
Algo for detecting a binomial in MATCH that is not in FOCAL <- species that don't exist in the tree
Sorting the leftovers in MATCH

# FOCAL LEFTOVER MATCHING
 - [ ] TODO: for Rob
Algo for "de matching" matches (e.g. Genus species could potentially match with Genus specias, specius)
Sorting the leftovers in FOCAL

# SORTING THE CONFIDENCE SCORES
Passing all the names through all the algorithms and give them some confidence score
