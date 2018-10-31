.yamlpretty <- function(node, clevels, nodeID) {
    if (node$Type == 'Terminal') {
        node$Size = sum(nodeID == node$ID)
        return(node)
    } else {
        if (node$Role != 'num') {
            node$ThreshSet = clevels[[node$SplitVar]][node$ThreshSet]
        }
        node$Left = .yamlpretty(node$Left, clevels, nodeID)
        node$Right = .yamlpretty(node$Right, clevels, nodeID)
        return(node)
    }
}

print.node <- function(node, depth = 0, digits = 3, long = TRUE, yName, trtName, tlevels, clevels, ...) {
    if (node$Type == 'Terminal') {
        cat(rep(' ', depth), 'ID: ', node$ID, ', Size: ', node$Size, ' [Terminal]\n', sep = '', ...)
        if (long) {
            cat(rep(' ', depth), 'Outcome Models: \n', sep = '', ...)
            for(i in seq_along(yName)) {
                cat(rep(' ', depth + 4), yName[i], rep(' ', 8), 'Est', rep(' ', 8), 'SE\n', sep = '', ...)
                # cat(rep(' ', depth + 4), format(c(yName[i], 'Estimate', 'SE'), justify = 'centre'), '\n', sep = '', ...)
                xvar <- node$FitIndex[[i]]
                coefT <- matrix(NA, length(tlevels), 2)
                rownames(coefT) <- paste0(trtName, '.', tlevels)
                colnames(coefT) <- c('Estimate', 'Std.Err')
                # if (length(xvar) > 0) {
                #     for (j in seq_along(xvar)) {
                #         cat(rep(' ', depth + 4), xvar[j], '    ',
                #             round(node$Parms[[i]][j], digits), '\n', sep = '', ...)
                #     }
                # }

                for (j in seq_along(tlevels)) {
                    coefT[j, 1] <- node$Trts[[i]][j]
                    coefT[j, 2] <- node$SEs[[i]][j]
                    cat(rep(' ', depth + 4), paste0(trtName,'.', tlevels[j]), '    ',
                        round(node$Trts[[i]][j], digits), '    ',
                        round(node$SEs[[i]][j], digits),'\n', sep = '', ...)
                    # cat(rep(' ', depth + 4), format(c(paste0(trtName,'.', tlevels[j]),
                    #                                   round(node$Trts[[i]][j], digits), round(node$SEs[[i]][j], digits)), justify="centre"), '\n', sep = '', ...)
                }
                cat(rep(' ', depth), rep('- ', 14), '\n', sep = '', ...)
            }
        }

    } else {
        cat(rep(' ', depth), 'ID: ', node$ID, ', ', sep = '', ...)
        if (node$Role == 'num') {
            if (node$MisDirection != 'A') {
                cat(node$SplitVar, ' <=', ifelse(node$MisDirection == 'L', '* ', ' '), round(node$Threshold, digits), '\n', sep = '', ...)
            } else {
                cat(node$SplitVar, ' = ', 'NA\n', sep = '', ...)
            }
        } else {
            if (node$MisDirection != 'A') {
                cat(node$SplitVar, ' = { ', paste0(node$ThreshSet, collapse = ', '), ifelse(node$MisDirection == 'L', ', NA', ''), ' }\n', sep = '', ...)
            } else {
                cat(node$SplitVar, ' = ', 'NA\n', sep = '', ...)
            }

        }
        print.node(node$Left, depth + 4, digits, long, yName, trtName, tlevels, clevels, ...)

        cat(rep(' ', depth), 'ID: ', node$ID, ', ' , sep = '', ...)
        if (node$Role == 'num') {
            if (node$MisDirection != 'A') {
                cat(node$SplitVar, ' >', ifelse(node$MisDirection == 'L', ' ', '* '), round(node$Threshold, digits), '\n', sep = '', ...)
            } else {
                cat(node$SplitVar, ' != ', 'NA\n', sep = '', ...)
            }
        } else {
            if (node$MisDirection != 'A') {
                varLevel = clevels[[node$SplitVar]]
                cat(node$SplitVar, ' = { ', paste0(varLevel[which(!varLevel %in% node$ThreshSet)], collapse = ', '), ifelse(node$MisDirection == 'L', '', 'NA'), ' }\n', sep = '', ...)
            } else {
                cat(node$SplitVar, ' != ', 'NA\n', sep = '', ...)
            }
        }
        print.node(node$Right, depth + 4, digits, long, yName, trtName, tlevels, clevels, ...)
    }
}

#' Print fitted regression tree
#'
#' @param mrsobj MrS object
#' @param digits digits pass to coefficent
#' @param details whether to print fitting details
#'
#' @export
print.guide <- function(mrsobj, digits = 3, details = FALSE, ...) {
    print.node(mrsobj$treeRes, depth = 0, digits, details,
               mrsobj$ynames, mrsobj$trtname, mrsobj$tLevels[[1]], mrsobj$cLevels, ...)
}
