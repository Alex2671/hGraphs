{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Monad(forever,join)
import System.Process.Typed
import System.Directory
import Graphics.Vega.VegaLite
import qualified Data.Massiv.Array as A
import Data.Massiv.Array.Delayed
import Data.Massiv.Array.Manifest
import Data.Massiv.Array.Numeric
import Data.Massiv.Core
import Data.Massiv.Core.Index
import Data.List as L
import qualified Data.ByteString as BS
import Data.ByteString.Internal(unpackChars)
import Control.Exception(try,SomeException,toException)

someFunc :: IO ()
someFunc = 	do
			putStrLn "round one!"
			act <- getLine 
			dir <- case act of
						"1" ->  listDirectory "F:/N064"
						"2" ->  listDirectory "F:/N003"
						"3"	->  listDirectory "F:/N005"
			name <- case act of
						"1" -> return "F:/N064/"
						"2" -> return "F:/N003/"
						"3" -> return "F:/N005/"			
			putStrLn "macro HD&QMC downloading..."
			hdQMCdat <- sequence $ map (\path ->  do
												fullFile <- (try $ BS.readFile $ name ++ path ++ "/" ++ "out.log" :: IO (Either IOError BS.ByteString))
												case fullFile of
													Left fail -> funOO
													Right contents -> funIO contents
									) dir
		   	putStrLn "--------------------->done!"
		   	putStrLn "analytics"
		   	anal <- sequence $ map (\path ->  do
												fullFile <- BS.readFile $ name ++ path ++ "/" ++ "up.dat" 
												let (meso,macro) = subStr1 506 fullFile
												putStrLn $ show (meso,macro)    
												return (meso,macro)
									) dir
		   	putStrLn "--------------------->done!"
		   	let sHD = L.sort $ changeMacro $ concatC anal $ addIndex (length dir) hdQMCdat 
		   	case name of 
		   				"F:/N064/" -> toHtmlFile "mathH/lol.html" $ plottingFun sHD 
						"F:/N003/" -> toHtmlFile "mathH/lol2.html" $ plottingFun $ changeMeso sHD
						"F:/N005/" -> toHtmlFile "mathH/lol3.html" $ plottingFun $ changeMeso sHD
		   	wrFile name 0 sHD  
		   	return ()
	where
		funOO :: IO (Double,Double,Double,Double) 
		funOO = do 
				putStrLn $ (show (99.0,99.0, 99.0,99.0)) 
				return (99.0,99.0, 99.0,99.0)
		funIO :: BS.ByteString -> IO (Double, Double, Double, Double) 
		funIO fullFile = do
				let (meso,density,fail, macro) = subStr02 fullFile
				putStrLn $ show  (meso,density,fail, macro)    
				return ( meso,  density, fail, macro)		

		subStr1 :: Int -> BS.ByteString -> (Double,Double)
		subStr1 ind  nam  =  (meso, macro)
			where
				meso = read $ unpackChars $ head $ BS.split 32 $ last $ BS.split 61 $ (\x -> x !! 505) $ BS.split 10 nam 
				macro = read $ unpackChars $ head $ BS.split 32 $ last $ BS.split 61 $ (\x -> x !! 506) $ BS.split 10 nam 

		subStr02 ::  BS.ByteString -> (Double, Double, Double,Double)
		subStr02 nam = (meso, density, fail, macro)
			where			
				density = read $ unpackChars $ BS.takeWhile (/= 44) $ (\x -> x !! 2) $ BS.split 61 $ (\x -> x !! 1) $ BS.split 10 nam
				fail = read $ unpackChars $ last $ BS.split  45 $ last $ BS.split 61 $ (\x -> x !! 18) $ BS.split 10 nam
				meso = read $ unpackChars $ head $ BS.split 43 $ last $ BS.split 61 $ (\x -> x !! 18) $ BS.split 10 nam	   
				macro = read $ unpackChars $ last $ BS.split 61 $ (\x -> x !! 20) $ BS.split 10 nam
		
		concatC :: [(Double,Double)] -> [(Int,Double,Double,Double,Double)]  -> [(Double,Int,Double,Double,Double,Double,Double)]
		concatC [] [] = []
		concatC ((mesoUP,macroUP):ys) ((ind,mesoOUT,densityOUT,failOUT,macroOUT):xs) = (mesoOUT,ind,densityOUT,failOUT,macroOUT,mesoUP,macroUP) : concatC ys xs
		addIndex :: Int -> [(Double,Double,Double,Double)] -> [(Int,Double,Double,Double,Double)]
		addIndex _ [] = []
		addIndex ind ((a,b,c,d):xs) = (ind,a,b,c,d) : addIndex (ind - 1) xs
		changeMeso :: [(Double,Int,Double,Double,Double,Double,Double)] -> [(Double,Int,Double,Double,Double,Double,Double)]
		changeMeso [] = []
		changeMeso ((macroUP,ind,densityOUT,failOUT,mesoOUT,mesoUP,macroOUT):xs) = (mesoUP,ind,densityOUT,failOUT,macroOUT,macroUP,mesoOUT) : changeMeso xs
		changeMacro :: [(Double,Int,Double,Double,Double,Double,Double)] -> [(Double,Int,Double,Double,Double,Double,Double)]
		changeMacro [] = []
		changeMacro ((mesoOUT,ind,densityOUT,failOUT,macroOUT,mesoUP,macroUP):xs) = (macroUP,ind,densityOUT,failOUT,mesoOUT,mesoUP,macroOUT) : changeMacro xs

		wrFile :: String -> Int -> [(Double,Int,Double,Double,Double,Double,Double)] ->  IO ()
		wrFile _ _ []  = return ()
		wrFile name ind a  = do
							case name of
								"F:/N064/" -> writeFile "64parts.dat" $ rep1 ind a "# macroUP macroOUT errorOUT \n"
								"F:/N003/" -> writeFile "3parts.dat" $ rep ind a "# macroUP mesoUP mesoOUT errorOUT\n"
								"F:/N005/" -> writeFile "5parts.dat" $ rep ind a "# macroUP mesoUP mesoOUT errorOUT\n"
							return () 
			where 
				rep :: Int -> [(Double,Int,Double,Double,Double,Double,Double)] -> String -> String
				rep _ [] _ = ""
				rep 0 a text = text ++ (rep (ind +1) a "")
				rep ind ((a,i,b,c,d,e,f):xs) _  = (show i) ++ " " ++ (show a) ++ " " ++ (show e) ++ " " ++ (show d) ++ " " ++ (show c) ++ "\n" ++ (rep (ind+1) xs "") 								
				rep1 :: Int -> [(Double,Int,Double,Double,Double,Double,Double)] -> String -> String
				rep1 _ [] _ = "" 
				rep1 0 a text = text ++ (rep1 (ind +1) a "")
				rep1 ind ((a,i,b,c,d,e,f):xs) _  = (show i) ++ " " ++ (show a) ++ " " ++ (show f) ++ " " ++ (show c) ++ "\n" ++ (rep1 (ind+1) xs "")	

		numTypeData :: Double -> [(Double,Int,Double,Double,Double,Double,Double)] -> [DataRow]
		numTypeData  _ []  = []
		numTypeData ind ((a,i,b,c,d,e,f):xs) | b < 0.029 = dataRow 
				           							[("Номер точки", Number ind) , ("тип" , Str "расчёт") , ("потенциал", Str "Леннард-Джонс") , ("Доля конденсата", Number f) ] 
				            							 $	 dataRow 
				           							[("Номер точки", Number ind) , ("тип" , Str "аналитика") , ("потенциал", Str "Леннард-Джонс") , ("Доля конденсата", Number a) ] 	
				            							 $ (numTypeData (ind+1) xs )
										  |	b > 0.029 && b < 0.19 = dataRow  
				           							[("Номер точки", Number ind) , ("тип" , Str "расчёт") , ("потенциал", Str "Диполи") , ("Доля конденсата", Number f) ] 
				           								 $  dataRow  
				           							[("Номер точки", Number ind) , ("тип" , Str "аналитика") , ("потенциал", Str "Диполи") , ("Доля конденсата", Number a) ]
				            							 $ (numTypeData (ind+1) xs ) 
				            			  | b == 99.0 = dataRow  
				           							[("Номер точки", Number ind) , ("тип" , Str "аналитика") , ("потенциал", Str "просчёт") , ("Доля конденсата", Number a) ]
				            							 $ (numTypeData (ind+1) xs ) 				 
					      				  |	otherwise 			  = dataRow  
				           							[("Номер точки", Number ind) , ("тип" , Str "расчёт") , ("потенциал", Str "Юкава") , ("Доля конденсата", Number f) ] 
				           							     $  dataRow  
				           							[("Номер точки", Number ind) , ("тип" , Str "аналитика") , ("потенциал", Str "Юкава") , ("Доля конденсата", Number a) ]
				            							 $ (numTypeData (ind+1) xs)
		plottingFun :: [(Double,Int,Double,Double,Double,Double,Double)] -> VegaLite
		plottingFun a =  let 		
									cluster = [ MName "потенциал", MmType Nominal , MScale [ SType ScLog, SRange (RStrings ["blue", "red","green"])]]
									buster = [ MName "тип", MmType Nominal ]
									dat =  dataFromRows []
									        $ (numTypeData 1.0 a) 
									enc = encoding
									        . position X [ PName "Номер точки", PmType Quantitative ]
									        . position Y [ PName "Доля конденсата",  PmType Quantitative]
									        . color cluster
									        . shape buster


									bkg = background "rgba(0, 0, 0, 0.05)"   

							   in toVegaLite [title "Зависимость доли конденсата" [], height 500, width 500, bkg, dat , mark Point [], enc [] ]
--}

									        --

{--
	putStrLn "Namefile(tsv-only):" >> getLine >>= BS.readFile >>= (\f -> forever $ do
			str1 <- getLine
			str2 <- getLine
			matX <- return $ toMatrix f
			putStrLn $ show matX
			toHtmlFile "mathH/lol.html" $ arrArgSelector matX (read str1 :: Int) str2
			--3 "-"
			--(read str1 :: Int) str2
			proc <- startProcess $ shell "cd mathH & lol.html"
			return () )
	where
		arrArgSelector :: A.Array P Ix2 Double -> Int -> String -> VegaLite
		arrArgSelector matX num "-" = let
									arY = sliceX matX $ num
									arX = A.makeVectorR D Seq (A.Sz1 $ initDim . unSz $ A.size matX) $ \ x -> fromIntegral x
								in graphOut arX arY
		arrArgSelector matX num1 num2 = let 	
										arX = sliceX matX $ num1 
										arY = sliceX matX $ ( read num2 :: Int)
								in graphOut arX arY   
		graphOut :: A.Array D Ix1 Double -> A.Array D Ix1 Double -> VegaLite
		graphOut arX arY  = let 
									dat =  dataFromColumns []
									        . dataColumn "Density" (Numbers $ A.toList arX)
									        . dataColumn "Condensate" (Numbers $ A.toList arY)
									enc = encoding
									        . position X [ PName "Density", PmType Quantitative ]
									        . position Y [ PName "Condensate", PmType Quantitative ]
									        . color [ MName "Origin" ]
									bkg = background "rgba(0, 0, 0, 0.05)"        		
							   in toVegaLite [height 1000 , width 1000, bkg, dat [], mark Circle [MTooltip TTEncoding], enc [] ]

		toMatrix :: BS.ByteString -> A.Array P Ix2 Double
		toMatrix word = A.fromLists' Seq $ Prelude.filter (\x -> x /= []) $ map (\x -> map read $ words $ unpackChars x) $ BS.split 10 word 	
		sliceX :: A.Array P Ix2 Double -> Int -> A.Array D Ix1 Double
		sliceX	arr n =   (A.transpose arr) A.!> n
	--}	 	

