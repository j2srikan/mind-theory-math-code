# Trying to implement the cognitve states program here



function test(x,y,z)
    return x + y + z
end



## Define multiplication and addition

function element_mult(x,y)
    if x==y
        return 1
    else
        return 0
    end
end



function mult(x,y)
    return broadcast(element_mult,x,y)
end


import Distributions.Binomial

function element_add(x,y,p)
    if x==y && x==0
        return 0
    elseif x==y && x==1
        return 1
    else
        return rand(Binomial(1,p))
    end
end



function add(x,y,p=0.5)
    return broadcast(element_add,x,y,p)
end


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


## Make the code to calculate R and L states, given a list of items

function makeR(items)
    return foldr(add, items)
end

function makeL(items)
    return foldl(add, items)
end




# Idea - all vectors between two given in hamming space is the number of permutations? Possible changes to get from one state to another?
