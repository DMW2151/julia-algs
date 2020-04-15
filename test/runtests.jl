using Test
using Random
using UUIDs

include("../src/Quack.jl") # Why TF does this work

const c, b = Quack.Coord(0., 0.), Quack.Box(0., 0., 1.)
const qtb = Quack.qtBox(c, 1)
const ε = 1/10000

@testset "Structure Init Tests - Coord" begin
    rng = MersenneTwister(2151)

    @test_nowarn Quack.Coord(0, 0); # Construct from Int
    @test_throws MethodError Quack.Coord('0', '0'); # Coord is constructed from Numeric Only
    
    @test_throws MethodError Quack.Coord(UUIDs.uuid4(rng), 0, 0); # User can't set UUID
end;

@testset "Structure Init Tests - Box" begin
    # Construct from Int vs. Float
    @test_nowarn Quack.Box(c, 1 ) 
    @test_nowarn Quack.Box(c, 1.) 

    # Box from Numeric Only and where S > 1
    @test_throws MethodError Quack.Box(c, "1") 
    @test_throws DomainError Quack.Box(c, -1)
end;

@testset "Structure Init Tests - qtBox" begin
    # Construct as in src; no children
    @test_nowarn Quack.qtBox(c, 1); 
    @test_throws DomainError Quack.qtBox(c, 0); # Numeric where S > 1
end;

# QTree Logic Tests 
@testset "checkPoint Tests" begin
    # Interior Points
    @test Quack.checkPoint(Quack.Coord(rand(), rand()), b) # Any rand pont on (0, 1)^2
    @test Quack.checkPoint(Quack.Coord(rand(), rand()), qtb) # Any rand pont on (0, 1)^2

    # If point is on SW/NE Corner - Include/Don't Include
    @test Quack.checkPoint(Quack.Coord(0, 0), qtb)
    @test Quack.checkPoint(Quack.Coord(1, 1), qtb) == false

    # If point is on Left/Right/North/South Edges 
    @test Quack.checkPoint(Quack.Coord(0., .5), qtb) # West Edge - Good
    @test Quack.checkPoint(Quack.Coord(.5, 0.), qtb) # South Edge - Good

    @test Quack.checkPoint(Quack.Coord(1., .5), qtb) == false # East Edge - Bad
    @test Quack.checkPoint(Quack.Coord(.5, 1.), qtb) == false # North Edge - Bad
    @test Quack.checkPoint(Quack.Coord(0., 1.), qtb) == false # North West Point - Bad
    @test Quack.checkPoint(Quack.Coord(0., 1. - ε), qtb) # Almost North West Point - Good
end;

@testset "regionContains Tests" begin
    # Box X contains X
    @test Quack.regionContains(qtb, b)     
end;

@testset "genRandomCoords Tests" begin
    randCoords = Quack.genRandomCoords(0., 0., 1., 1., 10)
    @test length(randCoords) == 10
end;

@testset "permissiveLen Tests" begin    
    # Nothing -> 0; Coord Array -> length(x)
    @test Quack.permissiveLen(Array{Quack.Coord}([c, c, c])) == 3
    @test Quack.permissiveLen(nothing) == 0

    # Only Coord Arrays OR Nothing
    @test_throws MethodError Quack.permissiveLen(Array{Int64}([1, 2, 4, 5]))
    @test_throws MethodError Quack.permissiveLen(Array{Quack.Coord}([c, nothing, c])) == 3
end;

@testset "getUUID Tests" begin
    @test typeof(Quack.getUUID(Array{Quack.Coord}([c, c, c]))) == Array{UUID,1}
end;

@testset "haversineDistance Tests" begin
    brooklynNY = Quack.Coord(-73.949997, 40.650002)
    snowshoePA = Quack.Coord(-77.949165, 41.027779)
    pacificWesternhemi = Quack.Coord(-179.99, 0.0)
    pacificEasternhemi = Quack.Coord(179.99, 0.0)
    NpoleRussia = Quack.Coord(50.0, 90.0 - ε)
    NpoleGreenland = Quack.Coord(-50.0, 90.0 - ε)

    # Sample Distances
    @test Quack.haversineDistance(brooklynNY, snowshoePA) ≈ 339.10590666
    @test Quack.haversineDistance(pacificEasternhemi, brooklynNY) ≈ 11357.982465065264
    @test Quack.haversineDistance(pacificWesternhemi, pacificEasternhemi) ≈ 2.2245268514
    @test Quack.haversineDistance(NpoleGreenland, NpoleRussia) ≈ 0.0170408643

    # Reflexive/Equality
    @test Quack.haversineDistance(pacificEasternhemi, pacificEasternhemi) == 0.0
    @test Quack.haversineDistance(snowshoePA, snowshoePA) == 0.0

    # d(x, z) <= d(x, y) + d(x, z) 
    @test Quack.haversineDistance(pacificEasternhemi, pacificWesternhemi) + Quack.haversineDistance(pacificWesternhemi, snowshoePA) > Quack.haversineDistance(pacificEasternhemi, snowshoePA)
end;

@testset "getSearchRange" begin
    q = Quack.getSearchRange(c, 1/√2)
    @test (q.SW.lng ≈ -0.4999999999999) && (q.SW.lat ≈ -0.4999999999999)
end;

@testset "queryRange" begin
    @test_nowarn Quack.queryRange(qtb, b); # No Error When Empty...
end;


@testset "radialSearch" begin
    circleCenter = Quack.Coord(0.5, 0.5)
    
    ## Inserts - Tree Build
    ps = Quack.genRandomCoords(0., 1., 0., 1., 100_000);
    sampleQTree = Quack.qtBox(Quack.Coord(.0, .0), 1.)

    for p in ps 
        Quack.insertIntoQuadTree!(sampleQTree, p) 
    end

    @test length(Quack.radialSearch(sampleQTree, circleCenter,  20.)) > 100
end;