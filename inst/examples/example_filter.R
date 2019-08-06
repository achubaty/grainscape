## Produce a patch-based MPG where patches are resistance features = 10
## and all patches are greater than or equal to 2 cells in size
filteredPatch <- patchFilter(tinyCost == 10, cells = 2)
tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
if (interactive()) plot(tinyPatchMPG)

## Compare to removal of patches greater than or equal to 40 cells in size!
filteredPatch <- patchFilter(tinyCost == 10, cells = 40)
tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
if (interactive()) plot(tinyPatchMPG)

## Use a rook/castle 4-direction case rather than the queen 8-direction case
## to identify neighbouring cells in a patch
filteredPatch <- patchFilter(tinyCost == 10, cells = 40, directions = 4)
tinyPatchMPG <- MPG(cost = tinyCost, patch = filteredPatch)
if (interactive()) plot(tinyPatchMPG)
