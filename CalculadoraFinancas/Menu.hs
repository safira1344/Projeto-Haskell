module Menu where

import System.IO
import Calculadora

lerNumero :: IO Double
lerNumero = do
  input <- getLine
  return (read input :: Double)

lerInteiro :: IO Int
lerInteiro = do
  input <- getLine
  return (read input :: Int)

menu :: IO ()
menu = do
  putStrLn "=== Calculadora de Finanças ==="
  
  putStrLn "2. Calcular Juros Simples"
  putStrLn "3. Calcular Juros Compostos"
  putStrLn "4. Simulador de Financiamento"
  putStrLn "5. Simulador de Rendimento"
  putStrLn "6. Projeção de Rendimento Anual"
  putStrLn "7. Sair"
  putStrLn "Escolha uma opção: "
  escolha <- getLine
  case escolha of
    
    "2" -> calcularJurosSimples
    "3" -> calcularJurosCompostos
    "4" -> simuladorDeFinanciamento
    "5" -> simuladorDeRendimento
    "6" -> projecaoDeRendimentoAnual
    "7" -> putStrLn "Saindo..."
    _   -> do
      putStrLn "Opção inválida!"

      menu

calcularJurosSimples :: IO ()
calcularJurosSimples = do
  putStrLn "--- Juros Simples ---"

  putStrLn "Digite o valor inicial: "
  valorInicial <- lerNumero
  putStrLn "Digite a taxa de juros (em %): "
  taxa <- lerNumero
  putStrLn "Digite o tempo em meses: "
  tempo <- lerInteiro
  let resultado = jurosSimples valorInicial taxa tempo
  putStrLn ("Resultado: " ++ show resultado)
  putStrLn ("Valor Total: " ++ show (resultado + valorInicial))
  
  menu

calcularJurosCompostos :: IO ()
calcularJurosCompostos = do
  putStrLn "--- Juros Compostos --- "

  putStrLn "Digite o valor inicial: "
  valorInicial <- lerNumero
  putStrLn "Digite o valor mensal: "
  valorMensal <- lerNumero
  putStrLn "Digite a taxa de juros (em %): "
  taxa <- lerNumero
  putStrLn "Digite o tempo em meses: "
  tempo <- lerInteiro
  let resultado = jurosCompostos valorInicial valorMensal taxa tempo
  putStrLn ("Resultado: " ++ show resultado)

  menu

simuladorDeFinanciamento :: IO ()
simuladorDeFinanciamento = do
  putStrLn "--- Simulador de Financiamento --- "

  putStrLn "Digite o valor do financiamento: "
  valor <- lerNumero
  putStrLn "Digite a taxa de juros anual (em decimal): "
  taxa <- lerNumero
  putStrLn "Digite o número de meses: "
  meses <- lerInteiro
  let parametros = ParametrosFinanciamento valor taxa meses
  let ResultadoSimulacao prestacao _ = simuladorFinanciamento parametros
  putStrLn ("Prestação mensal: " ++ show prestacao)

  menu

simuladorDeRendimento :: IO ()
simuladorDeRendimento = do
  putStrLn "--- Simulador de Rendimento --- "

  putStrLn "Digite o valor inicial: "
  valorInicial <- lerNumero
  putStrLn "Digite a taxa de rendimento anual (em decimal): "
  taxa <- lerNumero
  putStrLn "Digite a contribuição mensal: "
  contribuicao <- lerNumero
  putStrLn "Digite o número de meses: "
  meses <- lerInteiro
  let parametros = ParametrosRendimento valorInicial taxa contribuicao meses
  let ResultadoSimulacao valorFinal _ = simuladorRendimento parametros
  putStrLn ("Valor final: " ++ show valorFinal)

  menu

projecaoDeRendimentoAnual :: IO ()
projecaoDeRendimentoAnual = do
  putStrLn "--- Projeção de Rendimento Anual --- "

  putStrLn "Digite o valor inicial: "
  valorInicial <- lerNumero
  putStrLn "Digite a taxa de rendimento anual (em decimal): "
  taxa <- lerNumero
  putStrLn "Digite a contribuição mensal: "
  contribuicao <- lerNumero
  putStrLn "Digite o número de meses: "
  meses <- lerInteiro
  let parametros = ParametrosRendimento valorInicial taxa contribuicao meses
  let projecoes = projecaoRendimentoAnual parametros
  putStrLn "Projeção de Rendimento Anual:"
  mapM_ (\(mes, valor) -> putStrLn ("Mês " ++ show mes ++ ": " ++ show valor)) projecoes

  menu
