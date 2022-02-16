import Distributions.Binomial
using Plots

## Define standard values
N = 200 
k = 5.7

## Define multiplication and addition

# Multiplication
function element_mult(x,y)
    if x==y
        return 1
    else
        return 0
    end
end

function mult(x,y)
    return element_mult.(x,y)
end

# Addition
function element_add(x,y)
    if x==y && x==0
        return 0
    elseif x==y && x==1
        return 1
    else
        return rand(Binomial(1,0.5)) # set the value to be 0.5 for ease of using the add function
    end
end

function add(x,y)
    return element_add.(x,y)
end

## Hamming distance
function hamming(x,y)
    return sum(abs.(x-y))
end

## Similarity function
using Random, Distributions
function similarity(d)
    return exp(-k*cdf(Binomial(N,0.5),d))
end

## Make the code to calculate R and L states, given a list of items
function makeR(items)
    return foldr(add, items) # alternatively one could specify p here as well
end

function makeL(items)
    return foldl(add, items)
end

## Distance profiles with addition
function distance_sample(items) 
    state = zeros(N, 1) # Create array with N values of 0
    
    for i in 1:items # Add new states for each item
        new_state = rand(Binomial(), 200)
        state = [state new_state]
    end
    
    # List of items for addition
    add_states = [state[:,x] for x in 1:size(state)[2]] # Create vector of states to add
    R_addition = makeR(add_states) # Right associative addition
    L_addition = makeL(add_states) # Left associative addition
    rdist, ldist = [], []
    
    for i in 1:items
        rdist = push!(rdist, hamming(R_addition, state[:,i+1]))
        ldist = push!(ldist, hamming(L_addition, state[:,i+1]))
    end
    
    return rdist, ldist
end

# iterate through for number of samples
function distance_profiles(samples, items) 
    r_dist, l_dist = zeros(7,1), zeros(7,1)
    
    for i in 1:samples
        r_sample, l_sample = distance_sample(items)
        r_dist += r_sample/samples
        l_dist += l_sample/samples
    end

    return(r_dist, l_dist)
end

# sample + item specifications
samples = 500
items = 7

# run function to create data for distance graph
R_add, L_add = distance_profiles(samples, items)
LR_add = [R_add, L_add]

# plot distance graph
x = 1:items # x-axis
distance_plot = plot(x, LR_add/N, ylims = (0.2, 0.55), title = "Distance profiles for L and R states", label = ["d(x,R)" "d(x,L)"], xlabel = "Item", ylabel = "Distance", lw=3) # y-axis (divided by N elements in each state)
Plots.display(distance_plot)

## Define some candidate distance metrics (or measures)
import LinearAlgebra.dot
import LinearAlgebra.norm

# This is a true metric
function euclidean(x,y)
    return norm(x - y)
end

# so is this
function sqeuclidean(x,y)
    return sqrt(euclidean(x,y))
end

# should multiplication within the definition of a given distance measure follow standard, or the rules of this space?
# I think standard definition -- not like division is defined here anyways
# Not a true metric. But may be interesting to try since we are stuck in positive space anyways
function cosine(x,y)
    return 1 - (dot(x,y) / (norm(x) * norm(y)))
end

# same interest in angular
import Base.angle
import Base.acos

function angle(x,y)
    return acos(dot(x,y) / (norm(x) * norm(y)))
end

# Idea - all vectors between two given in hamming space is the number of permutations? Possible changes to get from one state to another?


# plot similarity graph
x = 1:items # x-axis
LR_sim = [similarity.(LR_add[1]), similarity.(LR_add[2])]
distance_plot = plot(x, LR_sim,  title = "Primacy and recency gradients for L and R states", label = ["S(d(x,R))" "S(d(x,L))"], xlabel = "Item", ylabel = "activation", lw=3) # y-axis (divided by N elements in each state)
Plots.display(distance_plot)
