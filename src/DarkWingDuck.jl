module DarkWingDuck

const QT_MAX_PTS = 8
const C = Float32(6372.8)

include("geomtypes.jl") # Import Custom DataTypes

"""
Check if point (p) is contained in a Box by comparing point to
the east and north of the box's SW point 
"""
function checkPoint(p::Coord, region::AbstractBox)::Bool
    # Check if point is to the east and north of the box's SW point 
    return (
        (p.lng > region.SW.lng) & (p.lng < region.SW.lng + region.sideLength) &&
        (p.lat > region.SW.lat) & (p.lat < region.SW.lat + region.sideLength)
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
    
    # Now that there are children, insert
    if r.children !== nothing
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

""" Name says it all, allow nothing..."""
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
    
    # The Work...
    if (r.children !== nothing) && childrenEmpty(r.children)
        r.children = Array{qtBox}([])
    end
    
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
Modified from:
    - https://www.movable-type.co.uk/scripts/latlong.html
"""
function haversineDistance(p1::Coord, plng::Float64, plat::Float64)::Float64
    d_lat = deg2rad(plat - p1.lat)
    A = (sin(d_lat/2) * sin(d_lat/2)) + cos(deg2rad(p1.lat)) * cos(deg2rad(plat)) * sin((plng - p1.lng)/2)^2
    return C * (2 * atan(sqrt(A), sqrt(1-A)))
end


""" Get the bounding box implied from a center coordinate and a radius """
function getSearchRange(c::Coord, radius::Float32)::Box
    return Box(Coord(c.lng - radius/√2 , c.lat - radius/√2), (2*radius)/√2)
end


""" Get all values in a given radius of a Coord """
function radialSearch(r::qtBox, c::Coord, d::Float32)::Array{Coord}
    # Get first square
    lng, lat = getLocation(c)
    return filter(x -> x = haversineDistance(x, lng, lat) < d, queryRange(r, getSearchRange(c, d)))
end 

end
