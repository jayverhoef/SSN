\name{Design functions}
\alias{binomialDesign}
\alias{poissonDesign}
\alias{hardCoreDesign}
\alias{systematicDesign}
\alias{noPoints}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Design functions}
\description{
Functions to generate points on a network.
}
\usage{
binomialDesign(n, replications=1, rep.variable = "Time", rep.values)
poissonDesign(lambda, replications=1, rep.variable = "Time", rep.values)
hardCoreDesign(n, inhibition_region, replications=1, rep.variable = "Time", rep.values)
systematicDesign(spacing, replications=1, rep.variable = "Time", rep.values)
noPoints
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
	A numeric vector having length 1 or the same length as the number of networks. This represents the number of points to be spread across a network.
}
  \item{lambda}{
	A numeric vector having length 1 or the same length as the number of networks. This represents the rate at which points occur on a network.
}
  \item{inhibition_region}{
	A numeric vector having length 1 or the same length as the number of networks. This represents the size of the inhibition region on a network.
}
	\item{spacing}{
	 A numeric vector having length 1 or the same length as the number of networks. This represents the desired spacing for the regular grid of points. 
	}
	\item{replications}{
		The number of replications of each point.
	}
	\item{rep.variable}{
		The name of the variable that will distinguish between the replicated points.
	}
	\item{rep.values}{
		The values that will be given to the variable named \code{rep.variable}. \code{rep.values[1]} will be assigned for replication 1, \code{rep.values[2]} for replication 2, etc. Defaults to \code{1:replications} if no value is given. 
	}
}
\details{
These design functions are intended to be used in the \code{obsDesign} or \code{predDesign} inputs of the \link{createSSN} function. The \code{binomialDesign} function represents a binomial process - A number \code{n[i]} of points are distributed randomly and uniformly across network \code{i} (or \code{n} points if \code{n} is a single number).

The \code{poissonDesign} function represents a poisson process, where points occur at rate \code{lambda[i]} on network \code{i} (or \code{lambda} if \code{lambda} is a single number).

The \code{hardCoreDesign} function represents a hard-core process where \code{n[i]} (or \code{n} if \code{n} has length 1) points are distributed uniformly and randomly on network \code{i}, and then points are removed until all points are at least \code{inhibition_region[i]} distant from each other (or \code{inhibition_region} if \code{inhibition_region} has length 1).

The \code{systematicDesign} function gives a deterministic and regular set of points. Starting from the outlet points are placed upwards along the network, at a fixed distance from the previous point. Note that while the generated grids are regular in a certain sense, they can appear non-regular at certains points from visual inspection. This is because it is impossible to generate a grid of truly equal-spaced points on a network. 

The noPoints function simply generates zero points across all networks. Note that this cannot be used as the design for the observed points as there must be at least one observed point. Also this is used directly without any parameters, unlike the other design functions.

	A design function must have the form
	\preformatted{
	function(tree.graphs, edge_lengths, locations, edge_updist, distance_matrices)}
	All inputs to the design function are lists of length \code{n} where \code{n} is the number of trees. Input \code{tree.graphs[[i]]} is 
	an object of class \code{igraph} which represent the ith generated network 
	in a graph theoretic sense; without any specific locations assigned to the 
	vertices. \code{edge_lengths[[i]]} contains the lengths of the edges for the 
	ith tree, in the same order as the edges appear in the corresponding 
	\code{igraph} object. Input \code{locations[[i]]} is a matrix with 
	\code{n[i]} rows and 2 columns giving the locations of the points on that 
	network. \code{edge_updist[[i]]} is a numeric vector which gives the upstream 
	distance from the downstream point of every stream segment, in the same order 
	as these edges appear in the corresponding \code{igraph} object. 
	\code{distance_matrices[[i]]} is a matrix with \code{n[i]} rows and columns, 
	giving the network distance between the downstream points of a pair of edges, 
	where the edges are ordered in the same way as in the \code{igraph} object. 
	To summarise, on tree number \code{i} if edge number \code{k} has downstream 
	point \code{k_} and edge number \code{l} has downstream point \code{l_} 
	then \code{edge_lengths[[i]][k]} gives the length of edge number \code{k}, 
	\code{edge_updist[[i]][k]} gives the distance from point \code{k_} to the 
	outlet of the stream network, and \code{distance_matrices[[i]][k, l]} gives 
	the network distance between points \code{k_} and \code{l_}. Note that some 
	of these inputs may have associated row or column names, but these should be 
	ignored. 
}
\value{
  A design function having the signature mentioned above. 
}

\author{
Rohan Shah \email{support@SpatialStreamNetworks.com}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{createSSN}}
}
