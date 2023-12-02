import numpy as np
from scipy.stats import mvn, qmc
from multiprocessing import Pool

def mcIntNDimSequential(f, lower, upper, muVector, covMatrix, RNG, nValues, df=None, numCores=2, chunkSize=None):
    nValuesPseudo = nValues
    nValues = min(nValues, 2**31-1)  # Limit of qrng functions

    nDim = len(upper)

    # Function to combine results from chunks
    def combine_chunks(*args):
        return np.concatenate(args)

    if RNG != "Pseudo":
        chunkSize = np.ceil(nValues / (numCores * nDim))
    else:
        chunkSize = np.ceil(nValuesPseudo / (numCores * nDim))

    def generate_chunk(i):
        if RNG == "Sobol":
            chunk = qmc.Sobol(dim=nDim, randomize=True).random(chunkSize)
            chunk = chunk.T * (upper - lower) + lower
        elif RNG == "Halton":
            chunk = qmc.Halton(dim=nDim).random(chunkSize)
            chunk = chunk.T * (upper - lower) + lower
        else:
            chunk = np.random.uniform(low=lower, high=upper, size=(chunkSize, nDim))
        return f(chunk, mean=muVector, sigma=covMatrix)

    with Pool(numCores) as pool:
        randomVariables = pool.map(generate_chunk, range(numCores))

    values = combine_chunks(*randomVariables)

    cumulativeSum = np.cumsum(values)
    sequentialEstimateVector = (np.prod(upper - lower) * cumulativeSum) / np.arange(1, len(cumulativeSum) + 1)

    return sequentialEstimateVector


def compareMCIntegrationMetrics(f, lower, upper, muVector, covMatrix, nValues, start=1, df=None):
    nValuesPseudo = nValues
    nValues = min(nValues, 2**31-1)

    if df is None:
        trueValue = mvn.mvnun(lower, upper, muVector, covMatrix)[0]
    else:
        raise NotImplementedError("Handling df parameter not implemented for this example")

    print("Sobol")
    sobolVector = mcIntNDimSequential(f, lower, upper, muVector, covMatrix, "Sobol", nValues)

    print("Halton")
    haltonVector = mcIntNDimSequential(f, lower, upper, muVector, covMatrix, "Halton", nValues)

    print("Pseudo")
    pseudoVector = mcIntNDimSequential(f, lower, upper, muVector, covMatrix, "Pseudo", nValues)

    estimateVector = {
        "Sobol": sobolVector[-1],
        "Halton": haltonVector[-1],
        "Pseudo": pseudoVector[-1]
    }

    varianceVector = {
        "Sobol": np.var(sobolVector),
        "Halton": np.var(haltonVector),
        "Pseudo": np.var(pseudoVector)
    }

    mseVector = {
        "Sobol": np.mean((trueValue - sobolVector)**2),
        "Halton": np.mean((trueValue - haltonVector)**2),
        "Pseudo": np.mean((trueValue - pseudoVector)**2)
    }

    calcTime = {
        "Sobol": sobolVector,
        "Halton": haltonVector,
        "Pseudo": pseudoVector
    }

    stdEstimateVector = {
        "Sobol": sobolVector[-1] - trueValue,
        "Halton": haltonVector[-1] - trueValue,
        "Pseudo": pseudoVector[-1] - trueValue
    }

    trueValueVector = {
        "Sobol": trueValue,
        "Halton": trueValue,
        "Pseudo": trueValue
    }

    estimateMatrix = {
        "Estimate": estimateVector,
        "Variance": varianceVector,
        "MSE": mseVector,
        "CalcTime": calcTime,
        "StdEstimate": stdEstimateVector,
        "TrueValue": trueValueVector
    }

    return estimateMatrix
