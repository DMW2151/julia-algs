using Random
using UUIDs


rng = MersenneTwister(2151)

""" AbstractType for 2D shapes w. 4 corners """
abstract type AbstractBox end

""" Basic point structure with lng -> x, and lat -> y """
struct Coord
    uuid::UUID # Woof - this is big; performance takes a big ole hit
    lng::Float64
    lat::Float64

    # Bound on Earth Coordinate System (-90, 90), (-180, 180)
    function Coord(lng::Float64, lat::Float64)
        uuid = UUIDs.uuid4(rng)
        new(uuid, lng, lat)
    end

    """ For Convenience """
    function Coord(lng::Union{AbstractFloat, Int}, lat::Union{AbstractFloat, Int})
        uuid = UUIDs.uuid4(rng)
        new(uuid, lat, lng)
    end

end

"""Used for search on qtBox, defined by the SW-corner and (SW.lng + side, SW.lat + side)"""
struct Box <: AbstractBox
    SW::Coord
    sideLength::Float64

    function Box(SW::Coord, s::Union{AbstractFloat, Int})
        @assert s > 0 throw(DomainError("Expects Side Length > 0; recieved $s "))
        new(SW, s)
    end 

    """ User makes no call to Coord """
    function Box(lng::Union{AbstractFloat, Int}, lat::Union{AbstractFloat, Int}, s::Union{AbstractFloat, Int})
        new(Coord(lng, lat), s)
    end

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
    
    """User should be able to define Children/Points on Init"""
    function qtBox(SW::Coord, s::Union{AbstractFloat, Int}, children::Array{qtBox}, points::Array{Coord})
        @assert s > 0 throw(DomainError("Expects Side Length > 0; recieved $s "))
        new(SW, s, children, points)
    end

    function qtBox(SW::Coord, s::Union{AbstractFloat, Int})
        @assert s > 0 throw(DomainError("Expects Side Length > 0; recieved $s "))
        new(SW, s, nothing, nothing)
    end
    
end
