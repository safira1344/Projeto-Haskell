module Calculadora where

descontoINSS :: Double -> Double
descontoINSS x
  | x < 1412 = x * 0.075
  | x < 2666.69 = x * 0.09
  | x < 4000.02 = x * 0.12
  | otherwise = x * 0.14


-- fazer o calculo pra colocar a porcentagem na taxa 
jurosSimples :: Double -> Double -> Int -> Double
jurosSimples valorInicial taxa tempo = valorInicial * (taxa/100) * fromIntegral(tempo)

-- Cálculo de juros compostos com função de alta ordem
-- iterate aplica a função \x -> x * (1 + taxa) repetidamente ao valor inicial capital. 
-- o operador !! é usado para extrair o n-ésimo elemento da lista resultante
-- jurosCompostos :: Double -> Double -> Int -> Double
-- jurosCompostos capital taxa n = (iterate (\x -> x * (1 + taxa)) capital) !! n

jurosCompostos :: Double -> Double -> Double -> Int -> Double
jurosCompostos valorInicial valorMensal taxaJuros periodo = calcularMes valorInicial periodo
  where
    taxaMensal = taxaJuros / 100
    calcularMes montante 0 = montante
    calcularMes montante n = calcularMes (montante * (1 + taxaMensal) + valorMensal) (n - 1)

--TODO fazer a taxa de juros que precisa para alcançar determinado valor de juros em um período específico

-- Tipo algébrico para os parâmetros de um financiamento
data ParametrosFinanciamento = ParametrosFinanciamento
  { valorFinanciamento :: Double
  , taxaJurosAnual :: Double
  , numeroMeses :: Int
  } deriving (Show)


-- Tipo algébrico para os parâmetros de um rendimento
data ParametrosRendimento = ParametrosRendimento
  { valorInicial :: Double
  , taxaRendimentoAnual :: Double
  , depositoMensal :: Double
  , numeroMesesRendimento :: Int
  } deriving (Show)


-- Tipo algébrico para os resultados das simulações
data ResultadoSimulacao = ResultadoSimulacao
  { valorFinal :: Double
  , detalheSimulacao :: String
  } deriving (Show)


-- Classe polimórfica para calculadora de rendimento
class CalculadoraRendimento a where
  calcularRendimento :: a -> ParametrosRendimento -> Double

-- Instância específica para cálculo de rendimento com SELIC
data CalculadoraSELIC = CalculadoraSELIC


instance CalculadoraRendimento CalculadoraSELIC where
  calcularRendimento CalculadoraSELIC (ParametrosRendimento valorInicial taxa depositoMensal numeroMesesRendimento) =
    jurosCompostos valorInicial depositoMensal taxa numeroMesesRendimento


-- Função para calcular a prestação do financiamento utilizando a fórmula Price com guardas
simuladorFinanciamento :: ParametrosFinanciamento -> ResultadoSimulacao
simuladorFinanciamento (ParametrosFinanciamento capital taxa periodos)
  | capital <= 0 = ResultadoSimulacao 0 "O valor do financiamento deve ser positivo."
  | taxa <= 0 = ResultadoSimulacao 0 "A taxa de juros deve ser positiva."
  | periodos <= 0 = ResultadoSimulacao 0 "O número de meses deve ser positivo."
  | otherwise = 
      let i = taxa / 12  -- Taxa mensal
          n :: Double
          n = fromIntegral periodos
          prestacao = capital * i / (1 - (1 + i) ** (-n))
      in ResultadoSimulacao prestacao "Prestação mensal calculada utilizando a fórmula Price"



-- Função para calcular o rendimento com contribuições regulares com guardas
simuladorRendimento :: ParametrosRendimento -> ResultadoSimulacao
simuladorRendimento (ParametrosRendimento capital taxa contribuicaoMensal periodos)
  | capital < 0 = ResultadoSimulacao 0 "O valor inicial deve ser não-negativo."
  | taxa < 0 = ResultadoSimulacao 0 "A taxa de rendimento deve ser não-negativa."
  | contribuicaoMensal < 0 = ResultadoSimulacao 0 "A contribuição mensal deve ser não-negativa."
  | periodos < 0 = ResultadoSimulacao 0 "O número de meses deve ser não-negativo."
  | otherwise = 
      let r = taxa / 12  -- Taxa mensal
          n = fromIntegral periodos
          valorFinal = (capital * (1 + r) ** n) + (contribuicaoMensal * ((1 + r) ** n - 1) / r)
      in ResultadoSimulacao valorFinal "Valor final do investimento com contribuições mensais regulares"


--fazer projeção de quanto vai render seu aporte durante 1 ano ( lista de tuplas ) que armazena a quantidade de meses e o rendimento
-- Função para projetar o rendimento mensal ao longo de um ano com guardas
projecaoRendimentoAnual :: ParametrosRendimento -> [(Int, Double)]
projecaoRendimentoAnual (ParametrosRendimento capital taxa contribuicaoMensal periodos)
  | capital < 0 = error "O valor inicial deve ser não-negativo."
  | taxa < 0 = error "A taxa de rendimento deve ser não-negativa."
  | contribuicaoMensal < 0 = error "A contribuição mensal deve ser não-negativa."
  | periodos < 0 = error "O número de meses deve ser não-negativo."
  | otherwise =
      let r = taxa / 12  -- Taxa mensal
          calcularMeses 0 valorAtual _ = []
          calcularMeses n valorAtual contribuicao =
            let novoValor = (valorAtual + contribuicao) * (1 + r)
            in (n, novoValor) : calcularMeses (n - 1) novoValor contribuicao
      in reverse (calcularMeses 12 capital contribuicaoMensal)