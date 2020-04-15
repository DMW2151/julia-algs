module Quack

const QT_MAX_PTS = 4

include("geomtypes.jl") # Import Custom DataTypes

"""Generate uniform random points along a surface"""
function genRandomCoords(x0::Float64, x1::Float64, y0::Float64, y1::Float64, n::Int)::Array{Coord}
    X = ((ones(n) * x0) + (rand(n) * (x1 - x0))) 
    Y = ((ones(n) * y0) + (rand(n) * (y1 - y0)))
    return [Coord(x, y) for (x,y) in zip(X,Y)]
end

"""
Check if point (p) is contained in a Box by comparing point to
the east and north of the box's SW point 
"""
function checkPoint(p::Coord, region::AbstractBox)::Bool
    # Check if point is to the east and north of the box's SW point 
    return (
        (p.lng >= region.SW.lng) & (p.lat >= region.SW.lat) && 
        (p.lng < region.SW.lng + region.sideLength) & (p.lat < region.SW.lat + region.sideLength)
    )
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
Split a qtBox into 4 equal parts
e.g. Assuming r has Coord(0. 0.) and sideLength = 2, 
subdivide!(r) will create new qtBoxes as follows:
    - ((0,0), 1), ((1,0), 1), ((0,1), 1), ((1,1), 1)
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
    end
    
    # If cannot push to parent and there are children, create children
    if r.children === nothing
        subdivide!(r) 
    end
    
    if r.children !== nothing # Now that there are children, insert
        for c in r.children 
            if insertIntoQuadTree!(c, ps)
                return true
            end
        end
    end
    return false # Fatal - Should not happen
end

""" Extract UUID """
function getUUID(c::Array{Coord})::Array{UUID}
    return map(x -> x.uuid, c)
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
    if !regionOverlap(r, qryBox)
        return pointsInRange # Exit if the range does not intersect this quad

    elseif regionContains(r, qryBox) 
        if r.points !== nothing # If  full region overlap then add all points w./o checks
            append!(pointsInRange, r.points) # Append parent points - No Check
        end
        
        if r.children !== nothing # Append children's Point's - No Check
            for c in r.children
                append!(pointsInRange, queryRange(c, qryBox))
            end
        end
    
    else # If partial overlap check each point on range 1-by-1  
        if r.points !== nothing
            for p in r.points
                if checkPoint(p, qryBox)
                    push!(pointsInRange, p)
                end
            end
        end
    
        if r.children === nothing # Append children's point's - With 1-by-1 check
            return pointsInRange # No children, exit
        else 
            for c in r.children # recursively Add the points from the children
                append!(pointsInRange, queryRange(c, qryBox))
            end
        end 
    end
    
    return pointsInRange 
 
end


getLocation = x::Coord -> (x.lng, x.lat)

"""Wrapper to getLocation function, extracts lat and lng attributes from AbstractBox"""
function coordRemoval!(r::qtBox, t::Coord)::Bool
    if getLocation(t) in map(getLocation, r.points)
        filter!(x -> x != t, r.points) # TODO: PLEASE MAKE SURE THIS IS POINTING TO THE ACTUAL OBJ
        return true
    else
        return false
    end

end

""" Name says it all, allow type nothing..."""
function permissiveLen(x::Union{Array{Coord},Nothing})::Int64
    if (typeof(x) === Nothing)  #Yuck
        return 0
    else
        return length(x)
    end
end
    
""" Wrapper to evaluate if children are empty """
function childrenEmpty(rs::Array{qtBox})::Bool
    return sum(x -> x = permissiveLen(x.points), rs) == 0
end

"""
Check Nodes where 1) points exist or existed, 2) children exist
meet condition:
    - (Array{dwd.Coord}([]) === nothing) = False
isassigned(Array{dwd.Coord}([])) = False
"""
function cleanUpQTree(r::qtBox)::Bool
    # Base Condition: Check If parent without Points has children without points
    if ((r.points === nothing) && (r.children === nothing))
        return true # Probably a new Tree
    end
    
    # Clear children
    if (r.children !== nothing) && childrenEmpty(r.children)
        r.children = Array{qtBox}([])
    end
    
    # If children not empty; check if grand-children empty...
    if (r.children !== nothing) && isassigned(r.children)
        for c in r.children 
            cleanUpQTree(c)
        end
    end
    
    return true
end

"""
Removes point from tree structure, find the point (t) and remove
WIP: Option to run intermediate cleaup on the parents of the removed point
see:
    - https://stackoverflow.com/a/9387997
"""
function removePoint!(r::qtBox, t::Coord)::Bool
    # Skip removing point if outside of parent range
    if !checkPoint(t, r)
        return false
    end
    
    # Check equality, check to see if coordinates are EXACTLY in parent's set of points
    if (r.points !== nothing) && isassigned(r.points)
        if coordRemoval!(r, t) 
            return true
        end
    end
            
    # If parent has children, recursively check to see if point is there
    if r.children !== nothing
        for c in r.children 
            if removePoint!(c, t)
                return true
            end
        end
    end
    
    return false #Could not remove point...
end

"""
Calculate Haversine Distance between two Coords. Formula 
calculates great-circle distance between two points 
Many thanks to: #https://github.com/quinnj/Rosetta-Julia/blob/master/src/Haversine.jl
"""
function haversineDistance(p1::Coord, p2::Coord)::Float64
    lat1, lng1 = p1.lat, p1.lng
    lat2, lng2 = p2.lat, p2.lng 
    return 2 * 6372.8 * asin(sqrt(sind((lat2-lat1)/2)^2 + cosd(lat1) * cosd(lat2) * sind((lng2 - lng1)/2)^2))
end


""" Get the bounding box implied from a center coordinate and a radius """
function getSearchRange(c::Coord, radius::Float64)::Box
    return Box(Coord(c.lng - radius/√2 , c.lat - radius/√2), (2*radius)/√2)
end


""" 
Check if any of the sides of a rectangle intersect a circle 
   - https://mathworld.wolfram.com/Circle-LineIntersection.html
Wolfram uses the following: determining factor: Delta=r^2d_r^2-D^2  
Where:
    - d_x =	x_2 - x_1
    - d_y =	y_2 - y_1
    - d_r =	sqrt(d_x^2+d_y^2)
    - D = det(x, y)
All of these get simplified when we use a square
"""
function intersectCircle(r::AbstractBox, c::Coord, d::Float64)::Bool
    if checkPoint(c, r) # First Check for if the center is in the box
        return true
    end 

    sL = r.sideLength
    deltaCoords = (r.SW.lng - r.SW.lat)    
    return (
        (d^2 * sL^2) > (sL * (deltaCoords - sL))^2 &&
        (d^2 * sL^2) > (sL * (deltaCoords + sL))^2
    )
end

""" 
Get all values in a given radius in Kilometers of a Coord 
https://stackoverflow.com/questions/401847/circle-rectangle-collision-detection-intersection
"""
function radialSearch(r::qtBox, c::Coord, d::Float64)::Array{Coord}
    # If intersection, then
    pointsInRange = Array{Coord}(Array[])
    
    if (!intersectCircle(r, c, d))
        return pointsInRange # Exit if the range does not intersect this quad
    end
    
    if r.points !== nothing # Haversine is super expensive compared to cartesian distance
        append!(pointsInRange, r.points)
    end

    if r.children === nothing # Append children's point's - With 1-by-1 check
        return pointsInRange # No children, exit
    else 
        for child in r.children # recursively Add the points from the children
            append!(pointsInRange, radialSearch(child, c , d))
        end
    end
    
    return filter!(x -> x = haversineDistance(x, c) < d, pointsInRange) # End of function catch...
end 

""" 
Nearest Neighbor Search 
    - http://homepage.divms.uiowa.edu/~kvaradar/sp2012/daa/ann.pdf
"""
function nnSearch(c::Coord)::Coord
    return c #Not yet Implemented
end

end
