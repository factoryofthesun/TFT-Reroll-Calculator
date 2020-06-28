import numpy as np
import matplotlib.pyplot as plt
from dataclasses import dataclass
import seaborn
import itertools
import matplotlib.font_manager

seaborn.set_palette(seaborn.color_palette('colorblind'))

UnitLevels = [1, 2, 3, 4, 5]
UnitPoolLive   = [29, 22, 16, 12, 10] # Number of each unique unit per cost
UnitPoolMidseason = [29, 22, 18, 12, 10] # Number of each unique unit per cost
NumUnitsLive = [12, 12, 12, 9, 7]   # Number of each unit cost
NumUnitsMidseason = [13, 13, 13, 10, 8]   # Number of each unit cost
UnitShopProbabilitiesLive = [       # Probability of finding a unit cost at each player level
    [1.00, 0.00, 0.00, 0.00, 0.00],
    [1.00, 0.00, 0.00, 0.00, 0.00],
    [0.75, 0.25, 0.00, 0.00, 0.00],
    [0.60, 0.30, 0.10, 0.00, 0.00],
    [0.40, 0.35, 0.20, 0.05, 0.00],
    [0.25, 0.35, 0.30, 0.10, 0.00],
    [0.19, 0.30, 0.35, 0.15, 0.01],
    [0.14, 0.20, 0.35, 0.25, 0.06],
    [0.10, 0.15, 0.25, 0.35, 0.15]
]
UnitShopProbabilitiesMidseason = [
    [1.00, 0.00, 0.00, 0.00, 0.00],
    [1.00, 0.00, 0.00, 0.00, 0.00],
    [0.75, 0.25, 0.00, 0.00, 0.00],
    [0.55, 0.30, 0.15, 0.00, 0.00],
    [0.40, 0.35, 0.20, 0.05, 0.00],
    [0.20, 0.40, 0.30, 0.10, 0.00],
    [0.14, 0.30, 0.40, 0.15, 0.01],
    [0.14, 0.20, 0.30, 0.30, 0.06],
    [0.10, 0.15, 0.25, 0.35, 0.15]
]

class UnitSettings:
    def __init__(self, cost, alreadyTaken, lookingFor, name = ""):
        self.Cost = cost
        self.AlreadyTaken = alreadyTaken
        self.LookingFor = lookingFor
        self.Name = name
        # self.CDF = list()
        # self.PDF = list()

# Returns a consistent multidimensional matrix index mapped onto 1D
def stateIndex(unitSettings: list, indices: list):
    index = 0
    mult = 1

    # Reversing the lists to help me think about it better. Not really needed.
    for unit, i in zip(reversed(unitSettings), reversed(indices)):
        index += mult * i
        mult *= unit.LookingFor + 1

    return index

# Calculates a probability matrix of finding each permutation of units specified by units *in a single shop slot*
# playerLevel: The level we are rolling at
# units: List of UnitSettings specifying the parameters for each unit
# othersTakenByCost: Nnumber of other units removed from the pool which we aren't searching for
# numShops: Number of shops to roll
def createSlotTransitionMatrix(playerLevel: int, units: list, othersTakenByCost: list, NumUnits = NumUnitsLive, UnitPool = UnitPoolLive, UnitShopProbabilities = UnitShopProbabilitiesLive):
    # N-dimensional matrix where N = len(units) + 1, flattened onto a 1-dimensional array
    matrixSize = np.product([unit.LookingFor + 1 for unit in units])

    # Markov chain transition matrix, initialized to 0
    transitionMatrix = np.zeros((matrixSize, matrixSize))

    # Probability of rolling each unit based on its cost
    unitCostProbabilities = [UnitShopProbabilities[playerLevel - 1][unit.Cost - 1] for unit in units]

    # Every possible permutation of the number of units we are looking for.
    # E.g. if we are looking for 3 of 2 different units, makes a list containing (0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1)...
    permutations = itertools.product(*[range(unit.LookingFor + 1) for unit in units])

    # For every possible unit number combination, add its entry to the transition matrix
    for numFound in permutations:
        # Consistent index for this unit combination
        index = stateIndex(units, numFound)

        # Array, indexed by unit cost, to keep track of how many of each cost are accounted for just by the permutation alone
        unitCostsFound = [0, 0, 0, 0, 0]
        for unitIndex in range(len(numFound)):
            unitCostsFound[units[unitIndex].Cost - 1] += numFound[unitIndex] + units[unitIndex].AlreadyTaken

        # For each unit in this permutation, add a transition to the matrix for finding 1 more
        for unitIndex in range(len(numFound)):
            # If we are looking for 2 of a unit, we need to go from (1, 0), (1, 1), (1, 2) to (2, 0), (2, 1), and (2, 2).
            # Without this if statement, the permutation list would also trip us over the index going from (1, 2) to (1, 3)
            # So let's just skip that case
            if numFound[unitIndex] + 1 > units[unitIndex].LookingFor:
                continue

            unit = units[unitIndex]
            unitsAvailable = UnitPool[unit.Cost - 1] - unit.AlreadyTaken - numFound[unitIndex]
            totalPool = UnitPool[unit.Cost - 1] * NumUnits[unit.Cost - 1] - othersTakenByCost[unit.Cost - 1] - unitCostsFound[unit.Cost - 1]

            # Probability a single shop slot contains this unit
            probability = unitCostProbabilities[unitIndex] * unitsAvailable / totalPool

            # Find the matrix index corresponding to +1 of this unit
            numFoundPlusOne = list(numFound)
            numFoundPlusOne[unitIndex] += 1
            indexPlusOne = stateIndex(units, numFoundPlusOne) # <-- index corresponding to +1 of this unit

            # The probability of staying at the same unit count is 1 - probability of finding any unit
            # Since we're only considering 1 shop slot here, the probability of finding a unit is additive
            if transitionMatrix[index, index] == 0:
                transitionMatrix[index, index] = 1 - probability
            else:
                transitionMatrix[index, index] -= probability
                assert transitionMatrix[index, index] >= 0

            assert probability > 0
            assert transitionMatrix[index, indexPlusOne] == 0
            transitionMatrix[index, indexPlusOne] = probability

    # Probability of staying in final state (= we found every unit) is 1, since we stop buying more units.
    lastIndex = stateIndex(units, [unit.LookingFor for unit in units])
    assert transitionMatrix[lastIndex, lastIndex] == 0
    transitionMatrix[lastIndex, lastIndex] = 1

    return transitionMatrix

def calculateProbabilityMatrixForNumShops(slotTransitionMatrix, numShops):
    rollTransitionMatrix = np.linalg.matrix_power(slotTransitionMatrix, numShops * 5)
    return rollTransitionMatrix

def matrixIndicesForNumOf(num, Units, unitsRequired = None, unitsExcluded = None):
    # yesno is used to create permutations
    yesno = itertools.chain(np.repeat(1, num), np.repeat(0, len(Units) - num))

    # permutations is every possibility that fits the condition of finding num
    # EXAMPLE: if we are looking for any 2 of 3 units, then permutations becomes {(1, 1, 0), (1, 0, 1), (0, 1, 1)}
    permutations = set(itertools.permutations(yesno))

    unitCombinationProbabilities = list()

    # Unique matrix indices
    allIndices = set()

    # First find the probability of finding each combination of units
    for p in permutations:
        if unitsRequired is not None:
            # Skip this permutation if it doesn't include a required unit
            if any([True if (unitsRequired[i] == True and p[i] == 0) else False for i in range(0, len(p))]):
                continue

        # ranges expands each 1 or 0 from permutations into a list of possibilities that satisfy the search condition
        # If 1, we need unit.LookingFor. If 0, we need anywhere from 0 to unit.LookingFor. If the unit is excluded and 0, we need up to unit.LookingFor - 1.
        ranges = [[unit.LookingFor] if needUnit == 1 else list(range(unit.LookingFor + 1)) for unit, needUnit in zip(Units, p)]

        # rangePermutations is every possible combination of unit numbers that satisfies the search condition.
        # If we are looking for 2 of 3 units, starting with 0, one such permutation is [(3, 3, 0), (3, 3, 1), (3, 3, 2), (3, 3, 3)]
        # IE, we must have 3 of the first two units, but we can have 0-3 of the third unit.
        rangePermutations = list(itertools.product(*ranges))

        for p in rangePermutations:
            assert len(p) == len(Units)
            if unitsExcluded is not None:
                if any([True if (unitsExcluded[i] == True and p[i] == Units[i].LookingFor) else False for i in range(0, len(p))]):
                    continue

            allIndices.add(stateIndex(Units, p))

    return allIndices

def probabilityFromMatrix(indices, matrix):
    # Look up the probability at each index
    # We are interested in the probability of going from 0 units found to the current condition, so the matrix is at row 0
    probabilities = [matrix[0, index] for index in indices]

    # The probabilities are disjoint. We can't have 1 and 2 of the same unit.
    # Therefore, the total probability is just the sum of the individual represented by each index.
    probability = sum(probabilities)

    return probability

def probabilityForNumOf(num, Units, transitionMatrix):
    indices = matrixIndicesForNumOf(num, Units)
    return probabilityFromMatrix(indices, transitionMatrix)

def PDFfromCDF(cdf):
    return [p1 - p2 for p2, p1 in zip(cdf, cdf[1:])]

# Boring code to color a bar graph in 3 different sections and print the boundaries
def plotProbabilityDistribution(ax, cdf):
    pdf = PDFfromCDF(cdf)
    xvals = list(range(1, len(pdf) + 1))

    normalstart = None
    lowrollstart = None

    for p, i in zip(cdf, range(0, len(cdf))):
        if p > 0.25 and normalstart is None:
            normalstart = i
        if p > 0.75 and lowrollstart is None:
            lowrollstart = i
            break

    normalx = normalstart if normalstart is not None else xvals[-1]
    lowrollx = lowrollstart if lowrollstart is not None else xvals[-1]

    rectsnormal = ax.bar(xvals[normalx : lowrollx], pdf[normalx : lowrollx], width = 1)
    rectslowroll = ax.bar(xvals[lowrollx:-1], pdf[lowrollx:-1], width = 1)
    rectshighroll = ax.bar(xvals[0:normalx], pdf[0:normalx], width = 1)

    rangelabel = ""
    if normalstart is not None and lowrollstart is not None:
        rangelabel = "50% of games\nbetween " + str(normalstart + 1) + " and " + str(lowrollstart) + " shops"
    elif normalstart is not None and lowrollstart is None:
        rangelabel = "50% of games\nbetween " + str(normalstart + 1) + " and >" + str(lowrollx) + " shops"
    elif normalstart is None:
        rangelabel = "50% of games\nrequire >" + str(normalx + 1) + " shops"

    ax.annotate(rangelabel, xy = (0.5, 0.95), xytext=(0, 5), xycoords = 'axes fraction', textcoords = 'offset points', fontweight='normal', ha = 'center', va ='top')
    ax.margins(x = 0, y = 0.2)

def plotCumulativeDistribution(ax, cdf):
    xvals = list(range(1, len(cdf) + 1))
    ax.bar(xvals, cdf, width=1)
    ax.margins(x = 0, y = 0.2)

# Utility function for creating columsn of different player levels with a given number of rows. Returns the matrix of axis objects.
def createSubplotGrid(figure, numRows, numCols):
    gs = plt.GridSpec(numRows, numCols, figure = figure)
    axes = list()

    for col in range(0, numCols):
        axes.append([])
        for row in range(0, numRows - 1):
            ax = figure.add_subplot(gs[row, col])
            ax.set_xticks([])
            axes[col].append(ax)
        ax = figure.add_subplot(gs[numRows - 1, col])
        ax.set_xlabel("Number of Shops")
        axes[col].append(ax)

    for row in range(0, numRows):
        axes[0][row].set_ylabel("Number of Games")

    for col in range(0, numCols):
        for row in range(0, numRows):
            axes[col][row].set_yticks([])

    return axes

def labelAxisAbove(ax, text):
    ax.annotate(text, xy = (0.5, 1.02), xytext=(0, 5), xycoords = 'axes fraction', textcoords = 'offset points', size = '12', fontweight='semibold', ha = 'center', va ='baseline')

def labelAxisLeft(ax, text):
    ax.annotate(text, xy = (0, 0.5), xytext = (-ax.yaxis.labelpad - 5, 0), xycoords=ax.yaxis.label, textcoords = 'offset points', size = '12', fontweight='semibold', ha='right', va='center', rotation=90)

def labelPlotInside(ax, text):
    ax.annotate(text, xy = (0.5, 0.95), xytext=(0, 5), xycoords = 'axes fraction', textcoords = 'offset points', fontweight='normal', ha = 'center', va ='top')

def FourCostCarry():
    MaxShops = 50

    figure = plt.figure(1, figsize = (12, 10), constrained_layout = True, dpi = 300)
    axes = createSubplotGrid(figure, 3, 5)

    pad = 5

    labelAxisAbove(axes[0][0], "Level 5")
    labelAxisAbove(axes[1][0], "Level 6")
    labelAxisAbove(axes[2][0], "Level 7")
    labelAxisAbove(axes[3][0], "Level 8")
    labelAxisAbove(axes[4][0], "Level 9")
    labelAxisLeft(axes[0][0], "1 specific unit")
    labelAxisLeft(axes[0][1], "1 of 2 specific units")
    labelAxisLeft(axes[0][2], "1 of 3 specific units")

    Kayle = UnitSettings(4, 1, 2, "Kayle")
    Irelia = UnitSettings(4, 1, 2, "Irelia")
    Jinx = UnitSettings(4, 1, 2, "Jinx")
    OthersTaken = [0, 0, 0, 0, 0]

    xvals = list(range(1, MaxShops))
    for level in range(0, 5):
        PlayerLevel = level + 5
        slotTransitionMatrix1 = createSlotTransitionMatrix(PlayerLevel, [Kayle], OthersTaken)
        slotTransitionMatrix2 = createSlotTransitionMatrix(PlayerLevel, [Kayle, Irelia], OthersTaken)
        slotTransitionMatrix3 = createSlotTransitionMatrix(PlayerLevel, [Kayle, Irelia, Jinx], OthersTaken)

        cdfs = [[], [], []]

        for numShops in range(1, MaxShops + 1):
            result1 = calculateProbabilityMatrixForNumShops(slotTransitionMatrix1, numShops)
            probability1 = probabilityForNumOf(1, [Kayle], result1)
            cdfs[0].append(probability1)

            result2 = calculateProbabilityMatrixForNumShops(slotTransitionMatrix2, numShops)
            probability2 = probabilityForNumOf(1, [Kayle, Irelia], result2)
            cdfs[1].append(probability2)

            result3 = calculateProbabilityMatrixForNumShops(slotTransitionMatrix3, numShops)
            probability3 = probabilityForNumOf(1, [Kayle, Irelia, Jinx], result3)
            cdfs[2].append(probability3)

        for row in range(0, 3):
            ax = axes[level][row]
            plotProbabilityDistribution(ax, cdfs[row])

    figure.suptitle("Finding a 4-cost unit", fontsize = 16, fontweight = 'semibold')

    figure.savefig('FourCostCarry.png')

def MissedFourCost():
    MaxShops = 50

    Kayle = UnitSettings(4, 1, 3, "Kayle")
    Irelia = UnitSettings(4, 0, 3, "Irelia")
    Units = [Kayle, Irelia]

    figure = plt.figure(2, figsize = (6, 4), constrained_layout = True, dpi = 300)
    ax = figure.add_subplot(1, 1, 1)
    ax.set_xlabel("Number of Shops")
    ax.set_ylabel("Probability")

    slotTransitionMatrix = createSlotTransitionMatrix(8, Units, [0, 0, 0, 0, 0])

    cdf = list()
    for i in range(1, MaxShops + 1):
        result = calculateProbabilityMatrixForNumShops(slotTransitionMatrix, i)
        # The probability of finding Irelia but not Kayle (exclude Kayle from allowed hits)
        indices = matrixIndicesForNumOf(1, Units, None, [True, False])
        p = probabilityFromMatrix(indices, result)
        cdf.append(p)

    plotCumulativeDistribution(ax, cdf)

    figure.suptitle("Ignoring an alternative 2* 4-cost", fontsize = 16, fontweight = 'semibold')
    figure.savefig('IgnoringFourCost.png')

def Mech():
    MaxShops = 140

    Kaisa = UnitSettings(2, 3, 6, "3* Kai'Sa")
    Annie = UnitSettings(2, 3, 6, "3* Annie")
    Rumble = UnitSettings(3, 3, 6, "3* Rumble")
    Fizz = UnitSettings(4, 1, 2, "2* Fizz")
    Units = [Kaisa, Annie, Rumble, Fizz]

    figure = plt.figure(3, figsize = (10, 10), constrained_layout = True, dpi = 300)
    axes = createSubplotGrid(figure, 3, 3)

    labelAxisAbove(axes[0][0], "Level 6")
    labelAxisAbove(axes[1][0], "Level 7")
    labelAxisAbove(axes[2][0], "Level 8")

    labelAxisLeft(axes[0][0], "Individual Units")
    labelAxisLeft(axes[0][1], "3* Kai'Sa + Level 7 Mech")
    labelAxisLeft(axes[0][2], "3* Kai'Sa + Level 8 Mech")

    for level in range(0, 3):
        individualCDFs = [[], [], [], []]
        mech7CDF = list()
        mech8CDF = list()

        PlayerLevel = level + 6
        slotTransitionMatrix = createSlotTransitionMatrix(PlayerLevel, Units, [0, 30, 15, 8, 0])

        for i in range(1, MaxShops + 1):
            result = calculateProbabilityMatrixForNumShops(slotTransitionMatrix, i)

            mech7Indices = matrixIndicesForNumOf(2, Units, [True, False, False, False])
            mech8Indices = matrixIndicesForNumOf(3, Units, [True, False, False, False])

            kaisaIndices = matrixIndicesForNumOf(1, Units, [True, False, False, False])
            annieIndices = matrixIndicesForNumOf(1, Units, [False, True, False, False])
            rumbleIndices = matrixIndicesForNumOf(1, Units, [False, False, True, False])
            fizzIndices = matrixIndicesForNumOf(1, Units, [False, False, False, True])

            individualCDFs[0].append(probabilityFromMatrix(kaisaIndices, result))
            individualCDFs[1].append(probabilityFromMatrix(annieIndices, result))
            individualCDFs[2].append(probabilityFromMatrix(rumbleIndices, result))
            individualCDFs[3].append(probabilityFromMatrix(fizzIndices, result))

            mech7CDF.append(probabilityFromMatrix(mech7Indices, result))
            mech8CDF.append(probabilityFromMatrix(mech8Indices, result))

        plotProbabilityDistribution(axes[level][1], mech7CDF)
        plotProbabilityDistribution(axes[level][2], mech8CDF)

        # individual unit probabilities
        xvals = list(range(1, len(individualCDFs[0])))
        axes[level][0].bar(xvals, PDFfromCDF(individualCDFs[1]), width = 1, alpha = 0.75, label = "3* Annie/3* Kai'Sa", color = '#bc658d')
        axes[level][0].bar(xvals, PDFfromCDF(individualCDFs[2]), width = 1, alpha = 0.75, label = "3* Rumble", color = '#f9d89c')
        axes[level][0].bar(xvals, PDFfromCDF(individualCDFs[3]), width = 1, alpha = 0.75, label = "2* Fizz", color = '#82c4c3')
        axes[level][0].legend(frameon = False)

    figure.savefig('Mech.png')

def Shredder():
    MaxShops = 100

    Xayah = UnitSettings(1, 11, 6, "Xayah")
    Jarvan = UnitSettings(1, 6, 6, "Jarvan")
    Fiora = UnitSettings(1, 6, 6, "Fiora")
    Caitlyn = UnitSettings(1, 6, 6, "Caitlyn")

    Units = [Xayah, Jarvan, Fiora, Caitlyn]

    figure = plt.figure(4, figsize = (6, 10), constrained_layout = True, dpi = 300)

    axes = createSubplotGrid(figure, 3, 2)

    labelAxisAbove(axes[0][0], "Level 4")
    labelAxisAbove(axes[1][0], "Level 5")

    labelAxisLeft(axes[0][0], "3* Xayah")
    labelAxisLeft(axes[0][1], "3* Xayah + 1 other 3*")
    labelAxisLeft(axes[0][2], "3* Xayah + 2 other 3*")

    xayahPlus0 = matrixIndicesForNumOf(1, Units, [True, False, False, False])
    xayahPlus1 = matrixIndicesForNumOf(2, Units, [True, False, False, False])
    xayahPlus2 = matrixIndicesForNumOf(3, Units, [True, False, False, False])

    for level in range(0, 2):
        PlayerLevel = level + 4

        slotTransitionMatrix = createSlotTransitionMatrix(PlayerLevel, Units, [60, 0, 0, 0, 0])

        cdfs = [[], [], []]

        for i in range(1, MaxShops + 1):
            result = calculateProbabilityMatrixForNumShops(slotTransitionMatrix, i)

            cdfs[0].append(probabilityFromMatrix(xayahPlus0, result))
            cdfs[1].append(probabilityFromMatrix(xayahPlus1, result))
            cdfs[2].append(probabilityFromMatrix(xayahPlus2, result))

        plotProbabilityDistribution(axes[level][0], cdfs[0])
        plotProbabilityDistribution(axes[level][1], cdfs[1])
        plotProbabilityDistribution(axes[level][2], cdfs[2])

    figure.savefig("Shredder.png")

def Level8vs9():
    MaxShops = 50

    Kayle = UnitSettings(4, 2, 1, "Kayle")
    Thresh = UnitSettings(5, 0, 1, "Thresh")
    MissFortune = UnitSettings(5, 0, 1, "Miss Fortune")
    Units = [Kayle, Thresh, MissFortune]

    figure = plt.figure(5, figsize = (8, 8), constrained_layout = True, dpi = 300)
    gs = plt.GridSpec(2, 2, figure = figure)

    axes = list()
    num1 = figure.add_subplot(gs[1, 0])
    num2 = figure.add_subplot(gs[1, 1], sharey = num1)
    cdf1 = figure.add_subplot(gs[0, 0], sharex = num1)
    cdf2 = figure.add_subplot(gs[0, 1], sharex = num2, sharey = cdf1)

    axes.append([cdf1, cdf2])
    axes.append([num1, num2])

    for legendariesRequired in range(0, 2):
        cdfAx = axes[0][legendariesRequired]
        numAx = axes[1][legendariesRequired]

        slotTransitionMatrix8 = createSlotTransitionMatrix(8, Units, [0, 0, 0, 0, 0])
        slotTransitionMatrix9 = createSlotTransitionMatrix(9, Units, [0, 0, 0, 0, 0])
        unitIndices = matrixIndicesForNumOf(legendariesRequired + 2, Units, [True, False, False])

        level8cdf = list()
        level9cdf = list()

        for numShops in range(1, MaxShops + 1):
            result8 = calculateProbabilityMatrixForNumShops(slotTransitionMatrix8, numShops)
            level8cdf.append(probabilityFromMatrix(unitIndices, result8))
            result9 = calculateProbabilityMatrixForNumShops(slotTransitionMatrix9, numShops)
            level9cdf.append(probabilityFromMatrix(unitIndices, result9))

        yvals = list()
        for i in range(0, 50):
            # Find the first equivalent level 8 shop
            equivalent8shop = 0
            result8cdf = 0
            while level9cdf[i] > result8cdf:
                equivalent8shop += 1
                result8 = calculateProbabilityMatrixForNumShops(slotTransitionMatrix8, equivalent8shop)
                result8cdf = probabilityFromMatrix(unitIndices, result8)

            yvals.append(equivalent8shop)

        numAx.plot(range(1, 51), yvals)
        labelPlotInside(numAx, "1 level 9 shop = " + str(yvals[-1]/50) + " level 8 shops")
        numAx.set_xlabel("Number of Level 9 Shops")

        xvals = list(range(1, MaxShops + 1))
        cdfAx.plot(xvals, level8cdf, label = "Level 8")
        cdfAx.plot(xvals, level9cdf, label = "Level 9")

        cdfAx.legend(frameon = False, loc = "lower right")

    labelAxisAbove(axes[0][0], "1 of 2 Legendaries")
    labelAxisAbove(axes[0][1], "2 of 2 Legendaries")

    axes[0][0].set_ylabel("Cumulative Probability")
    axes[1][0].set_ylabel("Number of Level 8 Shops for Better Odds")

    figure.suptitle("Finishing a 2* 4-cost and finding a legendary unit", fontsize = 16, fontweight = 'semibold')
    figure.savefig('Level8vs9.png')

def MidseasonCompare(unitCost, playerLevels, plotCDF = False, MaxShops = 100):
    MaxShops = 100

    playerLevelMin, playerLevelMax = playerLevels
    numPlayerLevels = playerLevelMax - playerLevelMin + 1

    figure = plt.figure(6 + unitCost + (6 if plotCDF else 0), figsize = (numPlayerLevels * 2.5, 6), constrained_layout = True, dpi = 300)

    axes = createSubplotGrid(figure, 2, numPlayerLevels)

    unit2star = UnitSettings(unitCost, 1, 2)
    unit3star = UnitSettings(unitCost, 3, 6)

    indices2star = matrixIndicesForNumOf(1, [unit2star])
    indices3star = matrixIndicesForNumOf(1, [unit3star])

    othersTaken = [0, 0, 0, 0, 0]

    for level in range(0, numPlayerLevels):
        playerLevel = level + playerLevelMin

        labelAxisAbove(axes[level][0], "Level " + str(playerLevel))

        matrix2starLive = createSlotTransitionMatrix(playerLevel, [unit2star], othersTaken)
        matrix3starLive = createSlotTransitionMatrix(playerLevel, [unit3star], othersTaken)
        matrix2starMidseason = createSlotTransitionMatrix(playerLevel, [unit2star], othersTaken, NumUnitsMidseason, UnitPoolMidseason, UnitShopProbabilitiesMidseason)
        matrix3starMidseason = createSlotTransitionMatrix(playerLevel, [unit3star], othersTaken, NumUnitsMidseason, UnitPoolMidseason, UnitShopProbabilitiesMidseason)

        cdf2starLive = list()
        cdf2starMidseason = list()
        cdf3starLive = list()
        cdf3starMidseason = list()

        for numShops in range(1, MaxShops + 1):
            result2starLive = calculateProbabilityMatrixForNumShops(matrix2starLive, numShops)
            result2starMidseason = calculateProbabilityMatrixForNumShops(matrix2starMidseason, numShops)
            result3starLive = calculateProbabilityMatrixForNumShops(matrix3starLive, numShops)
            result3starMidseason = calculateProbabilityMatrixForNumShops(matrix3starMidseason, numShops)

            cdf2starLive.append(probabilityFromMatrix(indices2star, result2starLive))
            cdf2starMidseason.append(probabilityFromMatrix(indices2star, result2starMidseason))
            cdf3starLive.append(probabilityFromMatrix(indices3star, result3starLive))
            cdf3starMidseason.append(probabilityFromMatrix(indices3star, result3starMidseason))

        pdf2starLive = PDFfromCDF(cdf2starLive)
        pdf2starMidseason = PDFfromCDF(cdf2starMidseason)
        pdf3starLive = PDFfromCDF(cdf3starLive)
        pdf3starMidseason = PDFfromCDF(cdf3starMidseason)

        color1 = seaborn.color_palette()[0]
        color2 = seaborn.color_palette()[4]

        if not plotCDF:
            xvals = range(1, MaxShops)

            axes[level][0].bar(xvals, pdf2starLive, label = "Live", width = 1, alpha = 0.75, color = color1)
            axes[level][0].bar(xvals, pdf2starMidseason, label = "Midseason", width = 1, alpha = 0.75, color = color2)
            axes[level][1].bar(xvals, pdf3starLive, label = "Live", width = 1, alpha = 0.75, color = color1)
            axes[level][1].bar(xvals, pdf3starMidseason, label = "Midseason", width = 1, alpha = 0.75, color = color2)
        else:
            xvals = range(1, MaxShops + 1)

            axes[level][0].plot(xvals, cdf2starLive, label = "Live", color = color1)
            axes[level][0].plot(xvals, cdf2starMidseason, label = "Midseason", color = color2)
            axes[level][1].plot(xvals, cdf3starLive, label = "Live", color = color1)
            axes[level][1].plot(xvals, cdf3starMidseason, label = "Midseason", color = color2)

            axes[level][0].set_ylim(0, 1)
            axes[level][1].set_ylim(0, 1)

    axes[0][0].legend(frameon = False)

    if plotCDF:
        axes[0][0].set_ylabel("Probability")
        axes[0][1].set_ylabel("Probability")

        axes[0][0].set_yticks([0, 0.25, 0.5, 0.75, 1])
        axes[0][1].set_yticks([0, 0.25, 0.5, 0.75, 1])

    labelAxisLeft(axes[0][0], "From 1 copy to 2*")
    labelAxisLeft(axes[0][1], "From 3 copies to 3*")

    figure.suptitle("Finding a " + str(unitCost) + "-cost unit", fontweight = 'semibold', size = '16')
    figure.savefig("Midseason_Tier" + str(unitCost) + ("_CDF" if plotCDF else "") + ".png")

FourCostCarry()
MissedFourCost()
Mech()
Shredder()
Level8vs9()
MidseasonCompare(1, (4, 7))
MidseasonCompare(2, (4, 8))
MidseasonCompare(3, (4, 9))
MidseasonCompare(4, (7, 9))
MidseasonCompare(5, (7, 9))
MidseasonCompare(3, (4, 9), True)
