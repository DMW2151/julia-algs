""" AbstractType for 2D shapes w. 4 corners """
abstract type AbstractBox end

""" Basic point structure with lng -> x, and lat -> y """
struct Coord
    lng::Float64
    lat::Float64
    
    # Bound on Earth Coordinate System (-90, 90), (-180, 180)
    function Coord(lng::Float64, lat::Float64)
        new(lng, lat)
    end
    
end

"""Used for search on qtBox, defined by the SW-corner and (SW.lng + side, SW.lat + side)"""
struct Box <: AbstractBox
    SW::Coord
    sideLength::Float64
end 


"""
Defined by the SW-corner and (SW.lng + side, SW.lat + side)
Same as Box. Adds set of points and set of children qtBoxes
"""
mutable struct qtBox <: AbstractBox
    SW::Coord
    sideLength::Float64
    children::Union{Array{qtBox}, Nothing} # Can Initialize on subdivide
    points::Union{Array{Coord}, Nothing} # Initialize on insert...
    
    """ 
    Option 1: User skips intermediate call to Box 
    User should NOT be able to define Children/Points on Init
    """
    function qtBox(SW::Coord, s::Union{AbstractFloat, Int})
        new(SW, s, nothing, nothing)
    end
    
end
