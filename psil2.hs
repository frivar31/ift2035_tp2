-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--                   
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""


---------------------------------------------------------------------------
-- Tables associatives simples                                           --
---------------------------------------------------------------------------

-- Type des tables indexées par des `α` qui contiennent des `β`.
-- Il y a de bien meilleurs choix qu'une liste de paires, mais
-- ça suffit pour notre prototype.
type Map α β = [(α, β)]

-- Recherche dans une table.
mmlookup :: Map Var β -> Var -> Maybe β
mmlookup [] _ = Nothing
mmlookup ((x,v) : xs) x' = if x == x' then Just v else mmlookup xs x'

mlookup :: Map Var β -> Var -> β
mlookup m x = case mmlookup m x of
                Just v -> v
                _ -> error ("Uknown variable: " ++ show x)

minsert :: Map Var β -> Var -> β -> Map Var β
minsert m x v = (x,v) : m

---------------------------------------------------------------------------
-- Représentation intermédiaire "Lambda"                                 --
---------------------------------------------------------------------------

type Var = String
type PrimType = String

-- Type Haskell qui décrit les types Psil.
data Ltype = Tprim PrimType
           | Tarw Ltype Ltype   -- Type "arrow" des fonctions.
           deriving (Show, Eq)

-- On défini un type séparé pour pouvoir lui donner une instance de `Show`
-- manuellement (`deriving` ne fonctionne pas avec les fonctions),
-- pour que `deriving` fonctionne lors de la définition de `Lexp`.
data Lelab = Lelab (Sexp -> Lexp)
instance Show Lelab where
    showsPrec _p _ = showString "<elabfun>"
data Delab = Delab (Sexp -> Ldec)
instance Show Delab where
    showsPrec _p _ = showString "<elabfun>"

-- Lpending et Dpending sont utilisés pour implanter le "currying":
-- lorsqu'on élabore une Sexp de la forme (Scons s1 s2), si `s1`
-- n'a pas assez d'arguments, l'élaboration renvoie un `Lpending`
-- (ou `Dpending`) auquel on peut alors passer `s2`.

-- Type Haskell qui décrit les expressions Psil.
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lhastype Lexp Ltype -- Annotation de type.
          | Lapp Lexp Lexp      -- Appel de fonction, avec un argument.
          | Llet Var Lexp Lexp  -- Déclaration de variable locale.
          | Lfun Var Lexp       -- Fonction anonyme.
          | Lif Lexp Lexp Lexp  -- Expression conditionnelle.
          | Lquote Value        -- Une Sexp immédiate.
          | Lpending Lelab      -- Nœud interne utilisé pendant l'élaboration.
          deriving (Show)

-- Type Haskell qui décrit les déclarations Psil.
data Ldec = Ddec Var Ltype      -- Déclaration globale.
          | Ddef Var Lexp       -- Définition globale.
          | Dpending Delab      -- Nœud interne utilisé pendant l'élaboration.
          deriving (Show)

-- Converti une Sexp Haskell en une Sexp Psil.
h2p_sexp :: Sexp -> Value
h2p_sexp (Snum n) = Vobj "num" [Vnum n]
h2p_sexp Snil = p_nil
h2p_sexp (Ssym s) = Vobj "sym" [Vstr s]
h2p_sexp (Scons s1 s2) = p_cons (h2p_sexp s1) (h2p_sexp s2)

-- Converti une Sexp Psil en une Sexp Haskell.
p2h_sexp :: Value -> Sexp
p2h_sexp (Vobj "num" [Vnum n]) = Snum n
p2h_sexp (Vobj "nil" []) = Snil
p2h_sexp (Vobj "sym" [Vstr s]) = Ssym s
p2h_sexp (Vobj "cons" [v1, v2]) = Scons (p2h_sexp v1) (p2h_sexp v2)
p2h_sexp (Vobj "moremacro" _) = error "moremacro inattendu"
p2h_sexp v = error ("Pas une Sexp: " ++ show v)

---------------------------------------------------------------------------
-- Conversion de Sexp à Lambda                                           --
---------------------------------------------------------------------------
-- On appelle aussi cette phase l'*élaboration*.

sexp2list :: Sexp -> [Sexp]
sexp2list sexp = s2l' sexp []
    where s2l' Snil res = res
          s2l' (Scons ses se) res = s2l' ses (se : res)
          s2l' se _ = error ("Liste inconnue: " ++ showSexp se)

s2t :: Sexp -> Ltype
s2t (Ssym pt) = Tprim pt
s2t (Scons (Scons ses (Ssym "->")) t2) =
    foldr (\ se -> Tarw (s2t se)) (s2t t2) (sexp2list ses)
s2t se = error ("Type Psil inconnu: " ++ (showSexp se))


-- Définitions des différentes formes spéciales.

sf_colon :: SpecialForm
sf_colon venv se =
    Lpending (Lelab (\ st -> Lhastype (s2l venv se) (s2t st)))

sf_fun :: SpecialForm
sf_fun venv (Ssym x) =
    Lpending (Lelab (\ e -> Lfun x (s2l venv e)))
sf_fun _ x = error ("devrait être un identifiant: " ++ showSexp x)


sf_if :: VEnv -> Sexp -> Lexp
sf_if venv (c) = 
        Lpending (Lelab (\ v->
            Lpending (Lelab (\f -> Lif (s2l venv c) (s2l venv v) (s2l venv f)))))

sf_if _venv _sc = error "¡¡COMPLÉTER!! sf_if"

sf_let :: SpecialForm
sf_let venv go =
    Lpending (Lelab (\ e -> llet venv go (s2l venv e)))

llet :: VEnv -> Sexp -> Lexp -> Lexp
--format simple
llet venv (Scons Snil (Scons (Scons Snil (Ssym x)) e1)) lexp =
    let l1 = s2l venv e1 in
    Llet x l1 lexp
--format complexe
llet venv (Scons go (Scons (Scons Snil (Ssym x)) e1)) lexp =
    let l1 = s2l venv e1 in
    llet venv go (Llet x l1 lexp)
llet _ _ _  = error "devrait être une liste de variables"


sf_quote :: SpecialForm
sf_quote _venv s = Lquote (h2p_sexp s)


-- Élaboration d'une Sexp qui est en position de "tête", i.e.
-- dans la partie gauche d'un `Scons`.
-- C'est nécessaire de gérer ce cas de manière différente pour
-- distinguer par exemple le cas d'usage de `fun` où on veut définir une
-- fonction et celui où on veut seulement faire référence à la
-- forme spéciale pour la stocker dans une autre variable.
h2l :: VEnv -> Sexp -> Lexp
h2l venv Snil = Lpending (Lelab (h2l venv))
h2l venv (s@(Ssym name)) =
    case mmlookup venv name of
      Just (Vsf _ sf) -> Lpending (Lelab (sf venv))
      Just (Vobj "macro" [Vfun macroexpander]) -> Lpending (Lelab (expansion venv macroexpander))
      _ -> s2l venv s

h2l venv (Scons s1 s2) =
    case h2l venv s1 of
      Lpending (Lelab ef) -> ef s2
      _ -> Lapp (s2l venv s1) (s2l venv s2)
h2l venv s = s2l venv s

expansion :: VEnv -> (Value -> Value) -> Sexp -> Lexp
expansion venv macroexpander s =
    let expanded = macroexpander (h2p_sexp s)
    in case expanded of
         Vobj "moremacro" [Vfun newMacroexpander] -> Lpending (Lelab (expansion venv newMacroexpander))
         _ -> s2l venv (p2h_sexp expanded)

-- Élaboration d'une Sexp qui n'est pas en position de "tête".
s2l :: VEnv -> Sexp -> Lexp
s2l _ (Snum n) = Lnum n
s2l _ (Ssym s) = Lvar s
s2l venv (s@(Scons _ _)) =
    case h2l venv s of
      Lpending _ ->error ("Argument manquant dans une macro ou forme spéciale")
      le -> le
s2l _ se = error ("Expression Psil inconnue: " ++ (showSexp se))


-- Élaboration des déclarations.
s2d :: VEnv -> Sexp -> Ldec
s2d venv Snil = Dpending (Delab (s2d venv))
s2d venv (Ssym "def") =
    Dpending (Delab (\ v ->
                     case v of
                       Ssym name ->
                         Dpending (Delab (\ e -> Ddef name (s2l venv e)))
                       _ -> error ("Pas un identifiant: " ++ show v)))
s2d _venv (Ssym "dec") =
    Dpending (Delab (\ v ->
                     case v of
                       Ssym name ->
                         Dpending (Delab (\ e -> Ddec name (s2t e)))
                       _ -> error ("Pas un identifiant: " ++ show v)))
s2d venv (Ssym x) =
  case mmlookup venv x of
      Just (Vobj "macro" [Vfun macroexpander]) -> Dpending (Delab (deexpansion venv macroexpander))
s2d venv (Scons s1 s2) =
    case s2d venv s1 of
      Dpending (Delab ef) -> ef s2
      _ -> error ("Argument en trop: " ++ show s2)
s2d _ se = error ("Déclaration Psil inconnue: " ++ showSexp se)

deexpansion :: VEnv -> (Value -> Value) -> Sexp -> Ldec
deexpansion venv macroexpander s =
    let vs = h2p_sexp s in
            let ves = macroexpander vs in
                case ves of
                    Vobj "moremacro" [Vfun newMacroexpander] -> Dpending (Delab (deexpansion venv newMacroexpander))
                    _ -> let es = p2h_sexp ves in
                            s2d venv es
---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

type TEnv = Map Var Ltype
type TypeError = String

-- `check Γ e τ` vérifie que `e` a type `τ` dans le contexte `Γ`.
check :: TEnv -> Lexp -> Ltype -> Maybe TypeError
check tenv (Lfun x e) (Tarw t1 t2) =
    check (minsert tenv x t1) e t2
check _ (Lfun _ _) t =
    error ("Type invalide pour Lfun (n'est pas de la forme t1->t2): " ++ show t)

check tenv (Llet x e1 e2 ) t =
  let e1' = check tenv e1 t
  in case e1' of
    Nothing -> e1'
    Just err -> error err

check tenv (Lif c v f) t
    | c' /= Nothing = c'
    | v' /= Nothing = v'
    | f' /= Nothing = f'
    | otherwise = Nothing
    where
        c' = check tenv c pt_bool
        v' = check tenv v t
        f' = check tenv f t

-- ¡¡COMPLÉTER!!
check tenv e t
  -- Essaie d'inférer le type et vérifie alors s'il correspond au
  -- type attendu.
  = let t' = synth tenv e
    in if t == t' then Nothing
       else Just ("Erreur de type: " ++ show t ++ " ≠ " ++ show t')

-- `synth Γ e` vérifie que `e` est typé correctement et ensuite "synthétise"
-- et renvoie son type `τ`.
synth :: TEnv -> Lexp -> Ltype
synth _    (Lnum _) = pt_int
synth tenv (Lvar v) = mlookup tenv v
synth tenv (Lhastype e t) =
    case check tenv e t of
      Nothing -> t
      Just err -> error err
synth tenv (Lapp e1 e2) =
    case synth tenv e1 of
      Tarw t1 t2 ->
          case check tenv e2 t1 of
            Nothing -> t2
            Just err -> error err
      _ -> error ("Not a function: " ++ show e1)
synth tenv (Llet x e1 e2) = synth (minsert tenv x (synth tenv e1)) e2
synth tenv (Lquote _) = pt_sexp



---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vstr String
           | Vfun (Value -> Value)
           | Vsf String SpecialForm
           | Vobj String [Value]

type VEnv = Map Var Value
type SpecialForm = VEnv -> Sexp -> Lexp

instance Show Value where
    showsPrec p  (Vnum n) = showsPrec p n
    showsPrec _p (Vstr s) = showString ("\"" ++ s ++ "\"")
    showsPrec _p (Vfun _) = showString "<Fonction>"
    showsPrec _p (Vsf name _) = showString ("<FormeSpéciale-" ++ name ++ ">")
    showsPrec _p (Vobj tag vals) =
        showString ("<" ++ tag ++ foldr (\ v s -> " " ++ show v ++ s)
                                        ">" vals)

-- Converti une fonction binaire Haskell en une primitive Psil.
binop :: (Int -> Int -> Int) -> Value
binop op = Vfun (\ x1 ->
                 case x1 of
                   Vnum v1 -> Vfun (\ x2 ->
                                   case x2 of
                                     Vnum v2 -> Vnum (v1 `op` v2)
                                     _ -> error "Erreur de type à l'exécution")
                   _ -> error "Erreur de type à l'exécution")

pt_int :: Ltype
pt_int = Tprim "Int"
pt_sf :: Ltype
pt_sf  = Tprim "SpecialForm"
pt_string :: Ltype
pt_string = Tprim "String"

pt_macro :: Ltype
pt_macro = Tprim "Macro"

pt_bool :: Ltype
pt_bool = Tprim "Bool"
p_true :: Value
p_true = Vnum 1
p_false :: Value
p_false = Vnum 0

pt_sexp :: Ltype
pt_sexp = Tprim "Sexp"
p_nil :: Value
p_nil = Vobj "nil" []
p_cons :: Value -> Value -> Value
p_cons hd tl = Vobj "cons" [hd, tl]


---------------------------------------------------------------------------
-- Environnement initial                                                 --
---------------------------------------------------------------------------

-- L'environnement initial qui contient les fonctions/constantes prédéfinies.
env0 :: [(Var, Ltype, Value)]
env0 =
    let pt_int_binop = Tarw pt_int (Tarw pt_int pt_int)
    in [("+", pt_int_binop, binop (+)),
        ("-", pt_int_binop, binop (-)),
        ("*", pt_int_binop, binop (*)),
        ("/", pt_int_binop, binop div),
        (":",   pt_sf, Vsf ":"   sf_colon),
        ("let", pt_sf, Vsf "let" sf_let),
        ("fun", pt_sf, Vsf "fun" sf_fun),
        -- Le type `Bool`.
        ("if",  pt_sf, Vsf "if"  sf_if),
        ("true",  pt_bool, p_true),
        ("false", pt_bool, p_false),
        ("zero?", Tarw pt_int pt_bool,
         Vfun (\ v -> case v of { Vnum 0 -> p_true; _ -> p_false })),
        -- Let type `Sexp`.
        ("shorthand-quote", pt_sf, Vsf "quote" sf_quote),
        ("nil", pt_sexp, p_nil),
        ("cons", Tarw pt_sexp (Tarw pt_sexp pt_sexp),
         Vfun (\ v1 -> Vfun (\ v2 -> p_cons v1 v2))),
        ("cons?", Tarw pt_sexp pt_bool,
         Vfun (\ v -> case v of { Vobj "cons" _ -> p_true; _ -> p_false })),
        ("nil?", Tarw pt_sexp pt_bool,
         Vfun (\ v -> case v of { Vobj "nil" _ -> p_true; _ -> p_false })),
        ("car", Tarw pt_sexp pt_sexp,
         Vfun (\ v -> case v of { Vobj "cons" [v1, _v2] -> v1; _ -> p_nil })),
        ("cdr", Tarw pt_sexp pt_sexp,
         Vfun (\ v -> case v of { Vobj "cons" [_v1, v2] -> v2; _ -> p_nil })),
        ("num", Tarw pt_int pt_sexp, Vfun (\ s -> Vobj "num" [s])),
        ("sym", Tarw pt_string pt_sexp, Vfun (\ s -> Vobj "sym" [s])),
        ("sym?", Tarw pt_sexp pt_bool,
         Vfun (\ v -> case v of { Vobj "sym" _ -> p_true; _ -> p_false })),
        ("sym-name", Tarw pt_sexp pt_string,
         Vfun (\ v -> case v of
                       Vobj "sym" [s] -> s
                       _ -> error "L'argument de sym-name n'est pas un `sym`")),
        -- Le type String.
        ("str-eq?", Tarw pt_string (Tarw pt_string pt_bool),
         Vfun (\ v1 -> Vfun (\ v2 -> case (v1, v2) of
                                     (Vstr s1, Vstr s2) | s1 == s2 -> p_true
                                     _ -> p_false))),
        -- Les macros.
        ("macro", Tarw (Tarw pt_sexp pt_sexp) pt_macro,
         Vfun (\expander -> Vobj "macro" [expander])),
        ("moremacro", Tarw (Tarw pt_sexp pt_sexp) pt_sexp,
         Vfun (\expander -> Vobj "moremacro" [expander]))]

tenv0 :: TEnv
tenv0 = map (\ (x,t,_) -> (x,t)) env0

venv0 :: VEnv
venv0 = map (\ (x,_,v) -> (x,v)) env0

-- La fonction d'évaluation principale.
eval :: VEnv -> Lexp -> Value
eval _venv (Lnum n) = Vnum n
eval venv (Lvar x) = mlookup venv x
eval venv (Lhastype e _) = eval venv e
eval venv (Lapp e1 e2) =
    let argValue = eval venv e2
    in case eval venv e1 of
        Vfun f -> f argValue
        other -> error ("Trying to call a non-function: " ++ show other)
eval venv (Llet x e1 e2) = eval (minsert venv x (eval venv e1)) e2
eval venv (Lfun x e) = Vfun (\ v -> eval (minsert venv x v) e)
eval _ (Lpending e) = error ("Expression incomplète: " ++ show e)
-- ¡¡COMPLÉTER!!
eval venv (Lif cond true false) =
    case eval venv cond of
        Vnum 0 -> eval venv false
        Vnum 1 -> eval venv true
        _ -> error ("pas un bool")
eval venv (Lquote e) = e

-- État de l'évaluateur.
type EState = ((TEnv, VEnv),       -- Contextes de typage et d'évaluation.
               Maybe (Var, Ltype), -- Déclaration en attente d'une définition.
               [(Value, Ltype)])   -- Résultats passés (en ordre inverse).

-- Évalue une déclaration, y compris vérification des types.
process_decl :: EState -> Ldec -> EState
process_decl (env, Nothing, res) (Ddec x t) = (env, Just (x,t), res)
process_decl (env, Just (x', _), res) (decl@(Ddec _ _)) =
    process_decl (env, Nothing,
                  error ("Manque une définition pour: " ++ x') : res)
                 decl
process_decl ((tenv, venv), Nothing, res) (Ddef x e) =
    -- Le programmeur n'a *pas* fourni d'annotation de type pour `x`.
    let ltype = synth tenv e
        tenv' = minsert tenv x ltype
        val = eval venv e
        venv' = minsert venv x val
    in ((tenv', venv'), Nothing, (val, ltype) : res)
process_decl ((tenv, venv), Just (x',ltype), res) (Ddef x e) =
    if x' /= x then
        process_decl ((tenv, venv), Nothing,
                      error ("Manque une définition pour: " ++ x') : res)
                     (Ddef x e)
    else
        -- Le programmeur a fourni une annotation de type pour `x`.
        let tenv' = minsert tenv x ltype
        in case check tenv' e ltype of
             Nothing ->
                 -- `venv'` et `val` sont mutuellement récursifs parce que
                 -- `e` peut être récursif (i.e. faire référence à `x`).
                 let venv' = minsert venv x val
                     val = eval venv' e
                 in ((tenv', venv'), Nothing, (val, ltype) : res)
             Just err -> ((tenv', venv), Nothing, error err : res)
process_decl _ (Dpending _) = error ("Argument manquant dans une déclaration")

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

process_sexps :: EState -> [Sexp] -> IO ()
process_sexps _ [] = return ()
process_sexps es (sexp : sexps) =
    let ((_, venv),_,_) = es
        decl = s2d venv sexp
        (env', pending, res) = process_decl es decl
    in do (hPutStr stdout)
            (concat
             (map (\ (val, ltyp) ->
                   "  " ++ show val ++ " : " ++ show ltyp ++ "\n")
              (reverse res)))
          process_sexps (env', pending, []) sexps

-- Lit un fichier contenant plusieurs Sexps, les évalue l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       let sexps = case parse pSexps filename filestring of
                     Left err -> error ("Parse error: " ++ show err)
                     Right es -> es
       process_sexps ((tenv0, venv0), Nothing, []) sexps

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l venv0 . sexpOf

typeOf :: String -> Ltype
typeOf = synth tenv0 . lexpOf

valOf :: String -> Value
valOf = eval venv0 . lexpOf
