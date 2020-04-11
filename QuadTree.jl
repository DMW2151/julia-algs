const QT_MAX_PTS = 4

""" AbstractType for 2D shapes w. 4 corners """
abstract type AbstractBox end

""" Basic point structure with lng(x), and lat(y) """
struct Coord 
    lng :: Float32
    lat :: Float32
end


"""
Used for search on qtBox, defined by the 
SW-corner and (SW.lng + side, SW.lat + side)
"""
struct Box <: AbstractBox
    SW :: Coord
    sideLength :: Float32
end 

"""
Generate uniform random points along a surface
e.g. generateRandUniform(0., 10., 0., 10., 1) -> Array{Float64,2}:  2.19137  1.57839
"""
function generateRandUniform(x0::Float64, x1::Float64, y0::Float64, y1::Float64, n::Int)::Array{Coord}
    X = ((ones(n) * x0) + (rand(n) * (x1 - x0))) 
    Y = ((ones(n) * y0) + (rand(n) * (y1 - y0)))
    return [Coord(x, y) for (x,y) in zip(X,Y)]
end

"""
Same as Box. Adds references to set of points and set of children qtBoxes

see: 
    - https://github.com/JuliaLang/julia/blob/master/base/pointer.jl
    - https://stackoverflow.com/questions/52329949/how-to-define-the-type-of-a-binary-search-tree-in-julia
"""
mutable struct qtBox <: AbstractBox
    SW :: Coord
    sideLength :: Float32 # sideLength > 0 
    children :: Union{Array{qtBox}, Nothing} #Initialize on subdivide
    points ::  Union{Array{Coord}, Nothing}
end

""" Default constructor initializes a qtBox with no children or points"""
qtBox(c::Coord, s::Float32) = qtBox(c, s, nothing, nothing); 


"""Check if point (p) is contained in a Box"""
function checkPoint(p::Coord, region::AbstractBox)::Bool
    # Check if point is to the east and north of the box's SW point 
    return (
        (p.lng > region.SW.lng) & (p.lng < region.SW.lng + region.sideLength) &&
        (p.lat > region.SW.lat) & (p.lat < region.SW.lat + region.sideLength)
    )
    end


"""
Split a qtBox into 4 equal parts
Assuming r has Coord(0. 0.) and sideLength = 2
e.g. subdivide!(r) create new qtBoxes as follows:
    - ((0,0), 1)
    - ((1,0), 1)
    - ((0,1), 1)
    - ((1,1), 1)

Split a node into the 4 subnodes evenly into squares
 _ _ _ _ 
|NW |NE | 
|_ _|_ _|
|SW |SE |
|_ _|_ _|

"""
function subdivide!(r::qtBox)
    
    if r.children === nothing # If not yet subdivided
        nodeDelta = r.sideLength / 2 # New sideLength
        
        # Add each of the children to Array...
        r.children = Array([
            qtBox(r.SW, nodeDelta), # S.W Child (+0, +0)
            qtBox(Coord(r.SW.lng + nodeDelta, r.SW.lat), nodeDelta), # S.E Child (+1 , +0),
            qtBox(Coord(r.SW.lng, r.SW.lat + nodeDelta), nodeDelta), #N.W child (+0, +1)
            qtBox(Coord(r.SW.lng + nodeDelta, r.SW.lat + nodeDelta), nodeDelta) # N.E child (+1, +1)
            ])
        end 
    end 


"""
Check if a Box (s1) wholly contains a qtBox (s0) 

Because qtBox is square, The only checks required are:
    - s1 SW point LESS (more west) than s0's
    - s1 SE point MORE (more east) than s0's
"""
function regionContains(s0::qtBox, s1::AbstractBox)::Bool
    return (s0.SW.lng >= s1.SW.lng) && 
        ((s0.SW.lng + s0.sideLength) <= (s1.SW.lng + s1.sideLength))
    end 


""" Check if a Box (s1) intersects/overlaps a qtBox (s0)"""
function regionOverlap(s0::qtBox, s1::Box)::Bool
    # If one rectangle is on left side of other 
    # OR If one rectangle is above other
    if (
        (s0.SW.lng > (s1.SW.lng + s1.sideLength)) ||
        (s1.SW.lng > (s0.SW.lng + s0.sideLength)) || 
        (s0.SW.lat > (s1.SW.lat + s1.sideLength)) || 
        (s1.SW.lat > (s0.SW.lat + s0.sideLength))
        )
        return false
    else 
        return true
    end
end


""" 
Recursively insert points into a qtBox
See Wikipedia description:
    - https://en.wikipedia.org/wiki/Quadtree#Pseudocode
"""
function insertIntoQuadTree!(r::qtBox, ps::Coord)::Bool        
        
    # Ignore if point is outside range of parent
    if !checkPoint(ps, r)
        return false
    end

    # If parent has room in point set and has not been subdivided, initialize 
    if r.points === nothing
        r.points = Array{Coord}([ps])
        return true
    
    # See const QT_MAX_PTS; If there is room in a parent point set, add to parent
    elseif (length(r.points) < QT_MAX_PTS)
        push!(r.points, ps)
        return true
    
    # If cannot push to parent and there are children, create children
    elseif r.children === nothing 
        subdivide!(r) 
    end
    
    # Now that there are children, insert
    if (r.children !== nothing)
        for c in r.children 
            if insertIntoQuadTree!(c, ps)
                return true
            end
        end
    end
    
    return false # Fatal - Should not happen
end


"""
Recursively query a range on a qtBox, includes a case for handling when
there is a complete overlap of the query space with the search space
see:
    - https://en.wikipedia.org/wiki/Quadtree#Pseudocode
"""
function queryRange(r::qtBox, qryBox::AbstractBox)::Array{Coord}

    # Initialize return list
    pointsInRange = Array{Coord}(Array[])
    
    # Exit if the range does not intersect this quad
    if !regionOverlap(r, qryBox)
        return pointsInRange

        
    elseif regionContains(r, qryBox) 
        # If  full region overlap then add all points w./o checks
        if r.points !== nothing
            # Append parent points - No Check
            append!(pointsInRange, r.points) 
        end
        
        # Append children's Point's - No Check
        if r.children !== nothing 
            for c in r.children
                append!(pointsInRange, queryRange(c, qryBox))
            end
        end
    
    else 
        # If partial overlap check each point on range 1-by-1    
        if r.points !== nothing
            for p in r.points
                if checkPoint(p, qryBox)
                    push!(pointsInRange, p)
                end
            end
        end
    
        # Append children's point's - With 1-by-1 check
        if r.children === nothing
            return pointsInRange # No children, exit
        else 
            # recursively Add the points from the children
            for c in r.children 
                append!(pointsInRange, queryRange(c, qryBox))
            end
        end
        
    end
    
    return pointsInRange 
end



getLocation = x::Coord -> (x.lat, x.lng)

"""Wrapper to getLocation function, extracts lat and lng attributes from AbstractBox"""
function partialCoordComp(ps::Array{Coord}, t::Coord)::Bool
    return getLocation(t) in map(getLocation, ps)
end


"""
Removes point from tree structure, find the point (t) and remove
WIP: Option to run intermediate cleaup on the parents of the removed point
see:
    - https://stackoverflow.com/a/9387997
"""
function removePointRebuild!(r::qtBox, t::Coord, modifyStructure::Bool)::Bool
    # Skip removing point if outside of parent range
    if !checkPoint(t, r) 
        return false
    end
    
    # Check equality, check to see if coordinates are EXACTLY in parent's set of points
    if r.points !== nothing
        if partialCoordComp(r.points, t) 
            filter!(x -> x != t, r.points)
            return true
        end
    end
    
    # WIP: CHECK THIS BLOCK
    if modifyStructure # TODO: Cobbled this together w. no external reference, plz check again...
        if r.children !== nothing
            for c in r.children # Search the space, each of children
                if c.points !== nothing
                    if partialCoordComp(c.points, t) 
                        filter!(x -> x != t, c.points) # if in child, remove and return true

                        # Modify Structure Sequence: If no children have points, 
                        # delete this qtBox's children by setting children back 
                        # to `Array{qtBox}([])`, TODO: Check Array{qtBox}([qtBox(Coord(1., 1.), 1.)])?
                        if sum(x -> x = (x.points !== nothing), r.children) == 0
                            r.children = Array{qtBox}([])
                        end

                        return true
                    end
                end
            end
        end
    end
        
    # If parent has children, recursively check to see if point is there
    if r.children !== nothing
        for c in r.children
            if removePointRebuild!(c, t, modifyStructure)
                return true
            end
        end
    end
    
    return false #Could not remove point...
end


"""
Calculate Haversine Distance between two Coords. Formula 
calculates great-circle distance between two points 
Modified from:
    - https://www.movable-type.co.uk/scripts/latlong.html
"""
function haversineDistance(p1::Coord, p2::Coord)::Float32
    lat1, lat2 = p1.lat, p2.lat    
    d_lat = deg2rad(lat2 - lat1)

    A = (sin(d_lat/2) * sin(d_lat/2)) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin((p2.lng - p2.lng)/2)^2

    return Float32(6372.8) * Float32(2 * atan(sqrt(A), sqrt(1-A)))
end


""" Get the bounding box implied from a center coordinate and a radius """
function getSearchRange(c::Coord, radius::Union{Float32, Float64})::Box
    return Box(Coord(c.lng - radius/√2 , c.lat - radius/√2), (2*radius)/√2)
    end


""" Get all values in a given radius of a Coord """
function radialSearch(r::qtBox, c::Coord, d::Union{Float32, Float64})::Array{Coord}
    # Get first square
    return filter(x -> x = haversineDistance(x, c) < d, queryRange(r, getSearchRange(c, d)))
end 