init_elements()
z.A = point(0, 0)
z.B = point(5, 0)
z.C = point(1, 4)
T.ABC = triangle:new(z.A, z.B, z.C)
T.EFG = T.ABC:medial()
z.E, z.F, z.G = T.EFG:get()
z.S = T.ABC:medial():circum_circle().south
z.O = T.ABC:medial().circumcenter
