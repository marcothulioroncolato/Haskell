{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Array
import Data.List (foldl')
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.Exception (evaluate)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

-- ============================================================
-- DADOS DE EXEMPLO (enunciado)
-- ============================================================

petr4Vetor :: Array Int Double
petr4Vetor = array (1,5) [(1,38.20), (2,38.80), (3,37.90), (4,39.10), (5,39.60)]

retornosMediosVetor :: Array Int Double
retornosMediosVetor = array (1,3) [(1,0.0092), (2,0.0068), (3,0.0086)]

pesosVetor :: Array Int Double
pesosVetor = array (1,3) [(1,0.40), (2,0.35), (3,0.25)]

petr4Lista :: [Double]
petr4Lista = [38.20, 38.80, 37.90, 39.10, 39.60]

retornosMediosLista :: [Double]
retornosMediosLista = [0.0092, 0.0068, 0.0086]

pesosLista :: [Double]
pesosLista = [0.40, 0.35, 0.25]

-- ============================================================
-- PARTE 1 - VETORES (Array Int Double)
-- ============================================================

retornosDiarioVetor :: Array Int Double -> Array Int Double
retornosDiarioVetor precos =
  let (lo, hi) = bounds precos
      n = hi - lo
  in if n <= 0
       then array (1,0) []
       else array (lo, lo + n - 1)
              [ (i, (precos ! (i+1) - precos ! i) / precos ! i)
              | i <- [lo .. hi - 1] ]

-- CORREÇÃO: Usando foldl' com índice para evitar alocação de listas intermediárias
mediaRetornoVetor :: Array Int Double -> Double
mediaRetornoVetor arr =
  let (lo, hi) = bounds arr
      n = hi - lo + 1
  in if n <= 0
       then 0
       else foldl' (\acc i -> acc + (arr ! i)) 0 [lo..hi] / fromIntegral n

-- CORREÇÃO: Usando foldl' com índice (O(1) real, sem listas escondidas)
produtoEscalarVetor :: Array Int Double -> Array Int Double -> Double
produtoEscalarVetor w r =
  let (lo, hi) = bounds w
  in foldl' (\acc i -> acc + (w ! i) * (r ! i)) 0 [lo..hi]

-- ============================================================
-- PARTE 2 - LISTAS ([Double])
-- ============================================================

retornosDiarioLista :: [Double] -> [Double]
retornosDiarioLista []  = []
retornosDiarioLista [_] = []
retornosDiarioLista xs  =
  zipWith (\p0 p1 -> (p1 - p0) / p0) xs (tail xs)

mediaRetornoLista :: [Double] -> Double
mediaRetornoLista [] = 0
mediaRetornoLista xs = foldl' (+) 0 xs / fromIntegral (length xs)

produtoEscalarLista :: [Double] -> [Double] -> Double
produtoEscalarLista ws rs = sum (zipWith (*) ws rs)

-- ============================================================
-- UTILITARIOS DE MEDICAO (Parte 3)
-- ============================================================

timeIt :: Double -> IO Double
timeIt x = do
  t0 <- getCPUTime
  _  <- evaluate x
  t1 <- getCPUTime
  pure (fromIntegral (t1 - t0) / 1e12)

listaParaArray :: [Double] -> Array Int Double
listaParaArray xs = listArray (1, length xs) xs

seriePrecos :: Int -> [Double]
seriePrecos n = scanl (\p d -> p * (1 + d)) 38.20 (take (n-1) variacoes)
  where
    variacoes = cycle [0.01, -0.008, 0.012, -0.005, 0.007]

pesosSimulados :: Int -> [Double]
pesosSimulados n = replicate n (1.0 / fromIntegral n)

retornosSimulados :: Int -> [Double]
retornosSimulados n = take n (cycle [0.0092, 0.0068, 0.0086])

-- ============================================================
-- DEMONSTRACAO (Partes 1 e 2)
-- ============================================================

demonstracao :: IO ()
demonstracao = do
  putStrLn "===== PARTE 1: VETORES (Array Int Double) ====="
  let retV = retornosDiarioVetor petr4Vetor
  putStrLn "(1) Retornos diarios:"
  print retV
  putStrLn "(2) Media de retorno:"
  printf "%.6f (%.4f%%)\n" (mediaRetornoVetor retV) (mediaRetornoVetor retV * 100)
  putStrLn "(3) Produto escalar:"
  printf "%.6f (%.4f%%)\n" (produtoEscalarVetor pesosVetor retornosMediosVetor) (produtoEscalarVetor pesosVetor retornosMediosVetor * 100)

  putStrLn ""
  putStrLn "===== PARTE 2: LISTAS ([Double]) ====="
  let retL = retornosDiarioLista petr4Lista
  putStrLn "(1) Retornos diarios:"
  print retL
  putStrLn "(2) Media de retorno:"
  printf "%.6f (%.4f%%)\n" (mediaRetornoLista retL) (mediaRetornoLista retL * 100)
  putStrLn "(3) Produto escalar:"
  printf "%.6f (%.4f%%)\n" (produtoEscalarLista pesosLista retornosMediosLista) (produtoEscalarLista pesosLista retornosMediosLista * 100)

-- ============================================================
-- PARTE 3 - COMPARATIVO DE DESEMPENHO
-- ============================================================

compararDesempenho :: IO ()
compararDesempenho = do
  let n = 1000000 :: Int

  putStrLn "=========================================================="
  putStrLn " CENARIO 1 - Serie Temporal (1.000.000 de precos)"
  putStrLn "=========================================================="

  let precosL = seriePrecos n
      precosA = listaParaArray precosL
      retL    = retornosDiarioLista precosL
      retA    = retornosDiarioVetor precosA

  tLRet <- timeIt (last retL)
  tARet <- timeIt (retA ! snd (bounds retA))
  tLMed <- timeIt (mediaRetornoLista retL)
  tAMed <- timeIt (mediaRetornoVetor retA)

  printf "Retornos diarios  -> Lista: %.6f s | Vetor: %.6f s\n" tLRet tARet
  printf "Media de retorno  -> Lista: %.6f s | Vetor: %.6f s\n" tLMed tAMed

  putStrLn ""
  putStrLn "=========================================================="
  putStrLn " CENARIO 2 - Carteira Massiva (1.000.000 de ativos)"
  putStrLn "=========================================================="

  let ws = pesosSimulados n
      rs = retornosSimulados n
      wa = listaParaArray ws
      ra = listaParaArray rs

  tLE <- timeIt (produtoEscalarLista ws rs)
  tAE <- timeIt (produtoEscalarVetor wa ra)

  printf "Produto escalar   -> Lista: %.6f s | Vetor: %.6f s\n" tLE tAE

  putStrLn ""
  putStrLn "=========================================================="
  putStrLn " ANALISE"
  putStrLn "=========================================================="
  putStrLn "* O tempo medido depende do hardware e do -O2 do GHC."
  putStrLn "  Nos resultados obtidos:"
  putStrLn "  - Retornos diarios: Array foi mais rapido (ou proximo)."
  putStrLn "  - Media e Produto escalar: Lista foi mais rapida."
  putStrLn ""
  putStrLn "* MOTIVO TECNICO:"
  putStrLn "  O Array tem acesso indexado O(1), o que e excelente."
  putStrLn "  Porem, o GHC e extremamente otimizado para listas."
  putStrLn "  Funcoes como 'sum (zipWith (*) ws rs)' sofrem FUSAO"
  putStrLn "  (fusion) e viram um unico laco de maquina sem alocar"
  putStrLn "  memoria intermediaria."
  putStrLn "  Ja o Array, para ser percorrido, precisa que criemos"
  putStrLn "  uma lista de indices [lo..hi] e acessemos via (!),"
  putStrLn "  e a funcao (!) faz checagem de limites a cada acesso."
  putStrLn ""
  putStrLn "* CONCLUSAO:"
  putStrLn "  Para acesso aleatorio, Array e superior."
  putStrLn "  Para percurso linear (somas, medias), Listas com"
  putStrLn "  zipWith e foldl' sao mais rapidas em Haskell devido"
  putStrLn "  a fusao de operacoes e otimizacao nativa do compilador."

-- ============================================================
-- MAIN
-- ============================================================

main :: IO ()
main = do
  setLocaleEncoding utf8
  demonstracao
  putStrLn "\nPressione ENTER para o comparativo de desempenho..."
  _ <- getLine
  compararDesempenho