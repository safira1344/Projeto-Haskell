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
  putStrLn "1. Calcular Desconto INSS"
  putStrLn "2. Calcular Juros Simples"
  putStrLn "3. Calcular Juros Compostos"
  putStrLn "4. Simulador de Financiamento"
  putStrLn "5. Simulador de Rendimento"
  putStrLn "6. Projeção de Rendimento Anual"
  putStrLn "7. Calculadora SELIC"
  putStrLn "8. Sair"
  putStrLn "Escolha uma opção: "
  escolha <- getLine
  case escolha of
    "1" -> calcularDescontoINSS
    "2" -> calcularJurosSimples
    "3" -> calcularJurosCompostos
    "4" -> simuladorDeFinanciamento
    "5" -> simuladorDeRendimento
    "6" -> projecaoDeRendimentoAnual
    "7" -> calcularSELIC
    "8" -> putStrLn "Saindo..."
    _   -> do
      putStrLn "Opção inválida!"

      menu

calcularDescontoINSS :: IO ()
calcularDescontoINSS = do
  putStrLn "--- Calcular Desconto INSS ---"
  putStrLn "Digite o salário bruto: "
  salario <- lerNumero
  let desconto = descontoINSS salario
  putStrLn ("Desconto INSS: " ++ show desconto)
  putStrLn ("Salário líquido: " ++ show (salario - desconto))

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
  let parametros = ParametrosRendimento valorInicial taxa contribuicao meses 0
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
  let parametros = ParametrosRendimento valorInicial taxa contribuicao meses 0
  let projecoes = projecaoRendimentoAnual parametros
  putStrLn "Projeção de Rendimento Anual:"
  mapM_ (\(mes, valor) -> putStrLn ("Mês " ++ show mes ++ ": " ++ show valor)) projecoes

  menu

calcularSELIC :: IO ()
calcularSELIC = do
  putStrLn "--- Simulador de investimento Tesouro SELIC ---"
  
  putStrLn "Digite o valor inicial: "
  valorInicial <- lerNumero
  putStrLn "Digite a taxa Selic anual (em decimal): "
  taxaSelicAnual <- lerNumero
  putStrLn "Digite a contribuição mensal: "
  depositoMensal <- lerNumero
  putStrLn "Digite o número de anos: "
  anos <- lerNumero

  let parametros = ParametrosRendimento valorInicial taxaSelicAnual depositoMensal 0 anos
  let rendimento = calcularRendimento CalculadoraSELIC parametros

  case anos of
    a | a <= 0.5 -> putStrLn ("Valor final após " ++ show a ++ " anos: " ++ show (((rendimento - valorInicial) * 0.775) + valorInicial))
    a | a <= 1 -> putStrLn ("Valor final após " ++ show a ++ " anos: " ++ show (((rendimento - valorInicial) * 0.8) + valorInicial))
    a | a <= 2 -> putStrLn ("Valor final após " ++ show a ++ " anos: " ++ show (((rendimento - valorInicial) * 0.825) + valorInicial))
    a | a > 2 -> putStrLn ("Valor final após " ++ show a ++ " anos: " ++ show (((rendimento - valorInicial) * 0.85) + valorInicial))
    _ -> putStrLn ("Número de anos inválido")

  menu
