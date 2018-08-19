-- Athus Assunção Cavalini
-- Hericles Bruno Quaresma Koelher

import Data.List
import Exemplos

--FUNÇÕES PRINCIPAIS:

-- QUESTÃO 1:
ocorreNoQuadro xs ys n | linhaP xs ys n = True
                       | colunaP xs ys n = True
                       | pertDiag xs ys n = True
                       | otherwise = False

-- QUESTÃO 2:
ocorremNoQuadro lpal cpal n cl = [ (x,custopal' x cl,(ocorreNoQuadro x cpal n)) | x<-lpal]

-- QUESTÃO 3:
nocorrencias lpal cpal n = sort [ (x,y) | (x,y)<-(nocorrencias0 lpal cpal n), y /= 0 ]

nocorrencias0 lpal cpal n = [ (x,nocorrenciast x (dividirlista cpal n) n) | x<-lpal ]

-- QUESTÃO 4:

palavrasmaiscaras lpal cpal n cl lim = inverteduplas (sort (inverteduplas ([(x,y) | (x,y)<-(palavrasmaiscaras1 lpal cpal n cl lim), y/=0])))

palavrasmaiscaras1 lpal cpal n cl lim = [ (x,(y*(nocorrenciast x (dividirlista cpal n) n))) | (x,y) <- (palavrasmaiscaras0 lpal cpal n cl lim), y >= lim]

palavrasmaiscaras0 lpal cpal n cl lim = [ (x,(custopal' x cl)) | x<-lpal]

-- QUESTÂO 5:
infoPalavrasEncontradas lpal cpal n = [ (x,y) | (x,y)<-(infoPalavrasEncontradas1 lpal cpal n), y /= []]

infoPalavrasEncontradas1 lpal cpal n = [ (x,(info x cpal n)) | x <- lpal]

-- QUESTÃO 6:
direcaoMaisComum lpal cpal n | maiscomum4 lpal cpal n == "h" = ["horizontal"]
                             | maiscomum4 lpal cpal n == "v" = ["vertical"]
                             | maiscomum4 lpal cpal n == "d" = ["daigonal"]
                             | maiscomum4 lpal cpal n == "hv" = ["vertical", "horizontal"]
                             | maiscomum4 lpal cpal n == "dv" = ["vertical", "diagonal"]
                             | otherwise = ["horizontal", "diagonal"]

--FUNÇÕES COMPLEMENTARES:

--Responde a transposta de uma matriz em forma de vetor.
transposta ys n = (concat(transpose(dividirlista ys n)))

--Dividr uma lista (matriz quadrada) em sublistas com o seu tamanho:
dividirlista' xs n x = [xs!!(z+n*x) | z <-[0..n-1]]


dividirlista xs n = map (dividirlista' xs n) [0..(n-1)]


--Checa se uma palavra pertence às linhas da matriz:
linhaP' xs ds = elem True (map (isInfixOf xs) ds)

linhaP xs ys n | linhaP' xs (dividirlista ys n)= True
               | linhaP' (reverse xs) (dividirlista ys n) = True
               | otherwise = False

--Checa se uma palavra pertence às colunas da matriz:
colunaP xs ys n | linhaP' xs (transpose (dividirlista ys n)) = True
                | linhaP' (reverse xs) (transpose (dividirlista ys n)) = True
                | otherwise = False

--Extrai as diagonais da matriz:
diagonal xs n x= [xs!!(x+z*(n+1))| z <- [0..(n-(x+1))]]


diag xs n =(map (diagonal xs n) [0..n-1])


diagonais xs n = union (diag xs n) (diag (transposta xs n) n)

--Checa se uma palavra pertence às diagonais da matriz:
pertDiag xs ys n | linhaP' xs (diagonais ys n) = True
                 | linhaP' (reverse xs) (diagonais ys n) = True
                 | otherwise = False

--Verifica quantas vezes uma letra aparece em uma string:
apareceu s xs = length [z |z <- xs, z == s]

--Forma uma lista de tuplas contendo a letra e quantas vezes ela apareceu
letra xs =[(x, apareceu x xs) | x <- ['A'..'Z']]

--Retira o segundo elemento de uma tupla em uma lista de tuplas
num xs = map (snd) xs

--Multiplica os elementos de 2 listas que se encontram na mesma posição
multi xs ys = [(xs!!i)*(ys!!i) | i <- [0..(length xs-1)]]

--Faz uma conversão para corrigir erro de tipo:
tconv xs = map (fromIntegral) xs

--Informa o custo da palavra (ou de uma lista de palavras):
custopal' xs cl = sum (multi (num (letra xs)) (tconv (num cl)))

--responde as posições de uma lista que ocorre em outra lista.
posicoesOc xs ys = [i | i<-[0..(length ys - length xs)], isPrefixOf xs (drop i ys)]

posicoesOci xs ys = [i+length xs | i<-[0..(length ys - length xs)], isPrefixOf (reverse xs) (drop i ys)]

--enumera uma lista.
enumera cpal = zip cpal [0..((length cpal)-1)]

--dá as posições de uma palavra que ocorre nas colunas de uma matriz.
posicoesOcc xs ys n = [ head [ y | (x,y)<-(enumera ys), (x,y) == ((transposta (enumera ys) n)!!i)] | i<-(posicoesOc xs (transposta ys n)) ]

posicoesOcci xs ys n = [ head [ y+(n)*((length xs)-1) | (x,y)<-(enumera ys), (x,y) == ((transposta (enumera ys) n)!!i) ] | i<-(posicoesOc (reverse xs) (transposta ys n))]

--dá as posições de uma palavra que ocorre nas diagonais de uma matriz.
posicoesOcd xs ys n = [ head [ y | (x,y)<-(enumera ys), (x,y) == (concat(diagonais (enumera ys) n))!!i ] | i<-(posicoesOc xs (concat(diagonais ys n))) ]

posicoesOcdi xs ys n = [ head [ y+((n+1)*((length xs)-1)) | (x,y)<-(enumera ys), (x,y) == (concat(diagonais (enumera ys) n))!!i ] | i<-(posicoesOc (reverse xs) (concat(diagonais ys n))) ]


--informa nas posições em que uma palavra aparece numa matriz.
infol pal cpal = [ (x,"horizontal-esquerda-direita") | x<-(posicoesOc pal cpal)]

infoli pal cpal = [ (x,"horizontal-direita-esquerda") | x<-(posicoesOci pal cpal)]

infoc pal cpal n = [ (x,"vertical-topo-base") | x<-(posicoesOcc pal cpal n)]

infoci pal cpal n = [ (x,"vertical-base-topo") | x<-(posicoesOcci pal cpal n)]

infod pal cpal n = [ (x,"diagonal-topo-base") | x<-(posicoesOcd pal cpal n)]

infodi pal cpal n = [ (x,"diagonal-base-topo") | x<-(posicoesOcdi pal cpal n)]

info pal cpal n = (infol pal cpal) ++ (infoli pal cpal) ++ (infoc pal cpal n) ++ (infoci pal cpal n) ++ (infod pal cpal n) ++ (infodi pal cpal n)

-- reponde as posições de uma lsita em uma lista de listas:
nocorrencias2 pal [] = []
nocorrencias2 pal cpal = posicoesOc pal (head cpal)++ nocorrencias2 pal (tail cpal)

-- quantas vezes uma palavra ocorre nas linhas
nocorrenciasl pal cpal = length (nocorrencias2 pal cpal)

-- quantas vezes uma palavra ocorre nas colunas
nocorrenciasc pal cpal= length (nocorrencias2 pal (transpose cpal))

-- quantas vezes uma palavra ocorre nas diagonais
nocorrenciasd pal cpal n = length (nocorrencias2 pal (diagonais (concat cpal) n))

-- quantas vezes uma palavra inversa ocorre nas linhas
nocorrenciasli pal cpal = length (nocorrencias2 (reverse pal) cpal)

-- quantas vezes uma palavra inversa ocorre nas colunas
nocorrenciasci pal cpal = length (nocorrencias2 (reverse pal) (transpose cpal))

-- quantas vezes uma palavra inversa ocorre nas diagonais
nocorrenciasdi pal cpal n = length (nocorrencias2 (reverse pal) (diagonais (concat cpal) n))

nocorrenciast pal cpal n = (nocorrenciasl pal cpal) + (nocorrenciasc pal cpal) + (nocorrenciasd pal cpal n) + (nocorrenciasli pal cpal) + (nocorrenciasci pal cpal) + (nocorrenciasdi pal cpal n)

-- inverte a posição dos elementos de uma lista de duplas:
inverteduplas xs = [ (y,x) | (x,y)<-xs]

--responde as direções de todas as palavras encontradas na lista.
maiscomum1 lpal cpal n = num (concat (num (infoPalavrasEncontradas lpal cpal n)))

--da lista anterior, responde apenas os primeiros caracteres de cada string ordenados e agrupados.
maiscomum2 lpal cpal n = group (sort(map (head) (maiscomum1 lpal cpal n)))

--adiciona a cada string o seu número de repetições e as coloca em ordem a partir destes.
maiscomum3 lpal cpal n = reverse(sort (zip (map (length) xs) xs))
                         where xs = (maiscomum2 lpal cpal n)

--pega o head da maior/maiores strings e coloca em uma só string.
maiscomum4 lpal cpal n = [head x | (y,x) <- (maiscomum3 lpal cpal n), y == maiorRep]
                          where maiorRep = fst (head (maiscomum3 lpal cpal n))
