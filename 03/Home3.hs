
-- | tribo_n = tribo_{n-1} + tribo_{n-2} + tribo_{n-3}
--   tribo_0 = 1
--   tribo_1 = 1
--   tribo_2 = 1
--
-- >>> tribo 0
-- 1
--
-- >>> tribo 1
-- 1
--
-- >>> tribo 2
-- 1
--
-- >>> tribo 3
-- 3
--
-- >>> odd (tribo 100)
-- True
tribo :: Int -> Integer
tribo n = helper n 1 1 1
    where helper 0 a b c = a
          helper 1 a b c = b
          helper 2 a b c = c
          helper n a b c = helper (n-1) b c (a+b+c) 

-- Тип Цвет, который может быть Белым, Черным, Красным, Зелёным, Синим, либо Смесь из трёх чисел (0-255)
-- операции Сложение :: Цвет -> Цвет -> Цвет
-- (просто складываются интенсивности, если получилось >255, то ставится 255)
-- ПолучитьКрасныйКанал, ПолучитьЗелёныйКанал, ПолучитьСинийКанал :: Цвет -> Int

data MyColor  = White 
              | Black 
              | Red 
              | Green 
              | Blue 
              | MyColor { red :: Int, green :: Int, blue :: Int }
              deriving Show

addition :: MyColor -> MyColor -> MyColor
addition c1 c2 = MyColor {  red = (if (getRedChannel c1 + getRedChannel c2) > 255 then 255 
                              else (getRedChannel c1 + getRedChannel c2))
                          , green = (if (getGreenChannel c1 + getGreenChannel c2) > 255 then 255 
                              else (getGreenChannel c1 + getGreenChannel c2))
                          , blue = (if (getBlueChannel c1 + getBlueChannel c2) > 255 then 255 
                              else (getBlueChannel c1 + getBlueChannel c2))}

getRedChannel :: MyColor -> Int
getRedChannel Red = 255
getRedChannel White =  255
getRedChannel Green = 0
getRedChannel Blue = 0
getRedChannel Black = 0
getRedChannel (MyColor r _ _ ) = r

getGreenChannel :: MyColor -> Int
getGreenChannel Red = 0
getGreenChannel White =  255
getGreenChannel Green = 255
getGreenChannel Blue = 0
getGreenChannel Black = 0
getGreenChannel (MyColor _ g _ ) = g

getBlueChannel :: MyColor -> Int
getBlueChannel Red = 0
getBlueChannel White =  255
getBlueChannel Green = 0
getBlueChannel Blue = 255
getBlueChannel Black = 0
getBlueChannel (MyColor _ _ b ) = b