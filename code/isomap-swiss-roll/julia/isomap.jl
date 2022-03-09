# create swiss roll dataset
import Distributions.MvNormal

d1 = rand(MvNormal([7.5,7.5], [1,1]),100)
d2 = rand(MvNormal([7.5,12.5], [1,1]),100)
d3 = rand(MvNormal([12.5,7.5], [1,1]),100)
d4 = rand(MvNormal([12.5,12.5], [1,1]),100)

using Plots
function plot_data()
    scatter(eachrow(d1)...)
    scatter!(eachrow(d2)...)
    scatter!(eachrow(d3)...)
    scatter!(eachrow(d4)...)
end

function make_roll(data)
    roll = Array{Float64}(undef, 3, 100)
    roll[1,:] = map(x -> x * cos(x), data[1,:])
    roll[2,:] = data[2,:]
    roll[3,:] = map(x -> x * sin(x), data[1,:])
    return roll
end

roll1 = make_roll(d1)
roll2 = make_roll(d2)
roll3 = make_roll(d3)
roll4 = make_roll(d4)
rolls = hcat(roll1, roll2, roll3, roll4)

swiss_roll = [rolls[:,i] for i in 1:size(rolls,2)]

# for visualizing swiss roll data
function plot_roll()
    scatter(eachrow(roll1)...)
    scatter!(eachrow(roll2)...)
    scatter!(eachrow(roll3)...)
    scatter!(eachrow(roll4)...)
end

# nearest neighbours for each point
using Distances

distance_matrix = [[euclidean(x, y) for x in swiss_roll] for y in swiss_roll] # find euclidean distances

function identify_strangers(x,k=7) # convert non-nearest neighbours to infinity
    strangers = sortperm(x)
    distant_values = strangers[k+1:size(x,1)]
    x[distant_values] .= Inf
    return(x)
end

distance_matrix = [identify_strangers(distance_matrix[x]) for x in 1:size(distance_matrix,1)]
distance_matrix = hcat(distance_matrix...) # convert from vector (of vectors) to matrix

heatmap(distance_matrix, color=:thermal) # Plot distance matrix

# Geodesic distance using Floyd-Warshall algorithm 
n = size(distance_matrix,1)

function shortest_path(dist) # not really sure why, but my implementation only seems to work half of the time
    for i in 1:n, j in 1:n
        if dist[j,i] != Inf
            for k in 1:n
                dist[j,k] = min(dist[j,k], dist[j,i] + dist[i,k]) 
                if dist[j,k] > dist[j,i] + dist[i,k]
                    print(min(dist[j,k], dist[j,i] + dist[i,k])) # for some inexplicable reason, there are apparently no points in the distance matrix where dist[j,k] is larger than dist[j,i] + dist[i,k]
                end
            end
        end
    end
return dist
end

distance_matrix = shortest_path(distance_matrix)

heatmap(distance_matrix, color=:thermal) # Plot distance matrix

# Multidimensional scaling
using LinearAlgebra
function mds(dist, p) # this will work with a distance matrix that doesn't still have Infs in it
    eig_vals, eig_vecs = eigvals(dist), eigvecs(dist)
    eig_vals = abs.(eig_vals)
    indices = sortperm(eig_vals)
    lg_indices = indices[n-p+1:n] # get largest eigenvectors/values
    
    return eig_vecs[:,lg_indices] * sqrt.(diagm(eig_vals[lg_indices])) # projection matrix
end

mds1_matrix = mds(distance_matrix, 1)
scatter(mds1_matrix) # Plot distance matrix

mds2_matrix = mds(distance_matrix, 2)
scatter(mds2_matrix) # Plot distance matrix