module Calculadora where

data Operacao = Soma | Subtracao | Multiplicacao | Divisao
    deriving (Show, Eq)

calcular :: Operacao -> Double -> Double -> Double
calcular Soma x y = x + y
calcular Subtracao x y = x - y
calcular Multiplicacao x y = x * y
calcular Divisao x y 
    | y /= 0 = x / y
    | otherwise = error "Divisao por zero não é permitida"


-- fazer o calculo pra colocar a porcentagem na taxa 
jurosSimples :: Double -> Double -> Int -> Double
jurosSimples valorInicial taxa tempo = valorInicial * (taxa/100) * fromIntegral(tempo)


jurosCompostos :: Double -> Double -> Int -> Double
jurosCompostos capital _ 0 = capital
jurosCompostos capital taxa n = jurosCompostos(capital * (1 + taxa)) taxa (n-1)

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
  calcularRendimento CalculadoraSELIC (ParametrosRendimento capital taxa _ periodos) =
    jurosCompostos capital taxa periodos


-- Função para calcular a prestação do financiamento utilizando a fórmula Price com guardas
simuladorFinanciamento :: ParametrosFinanciamento -> ResultadoSimulacao
simuladorFinanciamento (ParametrosFinanciamento capital taxa periodos)
  | capital <= 0 = ResultadoSimulacao 0 "O valor do financiamento deve ser positivo."
  | taxa <= 0 = ResultadoSimulacao 0 "A taxa de juros deve ser positiva."
  | periodos <= 0 = ResultadoSimulacao 0 "O número de meses deve ser positivo."
  | otherwise = 
      let i = taxa / 12  -- Taxa mensal
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