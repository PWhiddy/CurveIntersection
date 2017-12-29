{-# OPTIONS_GHC -O2 #-}

-- Failed attempt to test a loopy parametric equation for self-intersection with t between 0 and 2pi

main = print maximumSpacing

maximumSpacing = [(a,b,c) | a <- [2..9], b <- [2..9], c <- [2..9], closestIntersection (\t -> [sin(a*t),cos(b*t),cos(c*t)]) 0.03 0.06 > 0.1]
--maximumSpacing = maximum [ closestIntersection (\t -> [sin(a*t),cos(b*t),cos(c*t)]) 0.2 0.25 | a <- [2..15], b <- [2..15], c <- [2..15]]

closestIntersection :: (Double -> [Double]) -> Double -> Double -> Double
closestIntersection func stepSize spacing = 
    minimum $ map (minDistance func) [spacing, (spacing+stepSize)..(2*pi-stepSize)]
    where 
        minDistance f t = minimum $ map (dist (f t)) $ map f [0,stepSize..t-spacing]
            where dist a b = sqrt $ sum $ zipWith (\x y -> (x-y)**2) a b