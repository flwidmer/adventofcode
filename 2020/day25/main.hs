


card = 18356117
door = 5909654

solve1 = 
    let cardLoopSize = length $ takeWhile (/= card) $ scanl1 (loopIt 7) [1..10000000000]
        encryption = last $ scanl1 (loopIt door) [1..cardLoopSize + 1]
    in encryption
-- 3974372
-- 8623737

loopIt secret current _ =
    let s = current * secret
        r = rem s 20201227
    in r

