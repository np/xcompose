module NP.Unicode (greek, symbols, subscripts, superscripts, checkAmbs, disamb) where

import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Control.Applicative

np_specific :: [(String,String)]
np_specific =
  -- You can plug here snippet of text that you type often like
  -- names, addresses, phone numbers.
  [n "foo" "Foo Bar"
  ,n "foo@" "Foo.Bar@host.com"
  ]
  where n = (,)

greek :: [(String, String)]
greek = [(name, unicode) | (_,name,unicode) <- greekData] ++ 
        [ ([leading,shorthand],unicode)
        | (Just shorthand,_,unicode) <- greekData
        , leading                    <- ['\'', 'g'] ]

-- | Triples: (shorthand, name, unicode)
greekData :: [(Maybe Char, String, String)]
greekData = [(Just 'a', "alpha", "α")
            ,(Just 'b', "beta", "β")
            ,(Just 'g', "gamma", "γ")
            ,(Just 'G', "Gamma", "Γ")
            ,(Just 'd', "delta", "δ")
            ,(Just 'D', "Delta", "Δ")
            ,(Just 'e' , "epsilon", "ε")
            ,(Just 'z', "zeta", "ζ")
            ,(Just 'N' , "eta", "η") -- N is close to n which is graphically close
            ,(Just 'E' , "eta", "η") -- E is close to e which is the start of eta
            ,(Nothing , "theta", "θ")
            ,(Nothing , "Theta", "Θ")
            ,(Just 'i', "iota", "ι")
            ,(Just 'k', "kapa", "κ")
            ,(Just 'l', "lambda", "λ")
            ,(Just 'L', "Lambda", "Λ")
            ,(Just 'm', "mu", "μ")
            ,(Just 'n', "nu", "ν")
            ,(Just 'x', "xi", "ξ")
            ,(Just 'o', "omicron", "ο")
            ,(Just 'p' , "pi", "π")
            ,(Just 'P' , "Pi", "Π")
            ,(Just 'r', "rho", "ρ")
            ,(Just 's', "sigma", "σ")
            ,(Just 'S', "Sigma", "Σ")
            ,(Just 't', "tau", "τ")
            ,(Just 'f' , "phi", "φ")
            ,(Just 'F' , "Phi", "Φ")
            ,(Just 'c', "chi", "χ")
            ,(Just 'C', "Chi", "Χ")
            ,(Nothing , "psi", "ψ")
            ,(Nothing , "Psi", "Ψ")
            ,(Just 'w', "omega", "ω")
            ,(Just 'O', "Omega", "Ω")
            ]

accents :: [(String,String)]
accents = concat
  [a "aáàâäãå ȧ"
  ,a "b       ḃ"
  ,a "cć ĉ   çċ"
  ,a "d       ḋ"
  ,a "eéèêë   ė"
  ,a "f       ḟ"
  ,a "g       ġ"
  ,a "iíìîïĩ   "
  ,a "h       ḣ"
  ,a "m       ṁ"
  ,a "nń   ñ ņṅ"
  ,a "oóòôöõ  ȯ"
  ,a "p       ṗ"
  ,a "r       ṙ"
  ,a "s       ṡ"
  ,a "t       ṫ"
  ,a "uúùûü    "
  ,a "w       ẇ"
  ,a "x       ẋ"
  ,a "yý ŷÿ   ẏ"
  ,a "z       ż"
  ]
  where a [k, acute, agrave, acircum, adiar, atild, aring, aced, adot] =
          concat
          [b 'e' acute
          ,b '`' agrave
          ,b 'i' acircum
          ,b 'u' adiar
          ,b '~' atild
          ,b 'o' aring
          ,b ',' aced
          ,b '.' adot
          ] where c p k l = ([p,k],[l])
                  b _ ' ' = []
                  b p l   = [c p k l, c p (toUpper k) (toUpper l)]

parens :: [((String,String),(String,String))]
parens =
 -- parens
 [a "<"   "⟨"  ">"   "⟩"
 ,a "<<"  "⟪"  ">>"  "⟫"

-- These two confuse gnome-terminal.
 ,a "|("  "〖" ")|"  "〗"

 ,a "{|"  "⦃"  "|}"  "⦄"
 ,a "{{"  "⦃"  "}}"  "⦄"

 ,a "[["  "⟦"  "]]"  "⟧"

 ,a "|_"  "⌊"  "_|"  "⌋"
 ,a "r|_" "⌈"  "r_|" "⌉"
 ,a "rs|_" "⌜"  "rs_|" "⌝" -- s like small
 ]
 where a x y z t = ((x,y),(z,t))

symParen :: ((String,String),(String,String)) -> (String,String)
symParen ((x,y),(_,z)) = ('s':x, y ++ z)

symbols :: [(String, String)]
symbols =
 np_specific ++

 map fst parens ++ map snd parens ++ map symParen parens ++
 accents ++ circledChars ++

 [("99",  "«")
 ,("00",  "»")
 ,("90",  "«»")

 ,("  "," ")
 ,(" ."," ") -- sadly, I cannot highlight it in vim yet

 -- quantifiers
 ,("forall", "∀")
 ,("exists", "∃")
 ,("rA", "∀") -- reversed A
 ,("rE", "∃") -- reversed E
 ,("/rE", "∄")
 ,("r/E", "∄")
 ,("na", "∇") -- "na"bla
 ,("rgD", "∇") -- reversed Δ
 ,("r'D", "∇") -- reversed Δ
 ,("sum", "∑") -- sum is different from sigma Σ

 -- operators
 ,("<|","◃")
 -- ,("<|","◁") alternative
 ,("|>","▹")
 ,("b|>","▸")
 -- ,("|>", "▷")
 ,("><","⋈")
 ,("<)", "◅")
 ,("(>", "▻")
 ,("v","∨")
 ,("u","∪")
 ,("n","∩")
 ,("V","⋁")
 ,("+-", "±")
 ,("+u","⊎")
 ,("u+","⊎")
 ,("u[]","⊔")
 ,("n[]","⊓")
 ,("^^","̂") -- COMBINING CIRCUMFLEX ACCENT: ̂ (U+0302)
 ,("^","∧")
 ,("/\\", "∧")
 ,("\\/", "∨")
 ,("c\\/", "⋎") -- 'c' for curly
 ,("o","∘")
 ,(".","·")
 ,("...", "…")
 ,("c...", "⋯") -- 'c' for centered
 ,("v...", "⋮") -- 'v' for vertical
 ,("x","×")
 ,("neg","¬")
 ,("-.","∸")
 ,("-:","÷")
 ,("sqrt","√") -- square root
 ,("cbrt","∛") -- cube   root
 ,("fort","∜") -- fourth root
 ,("<w>", "◇")
 ,("<b>", "◈")

 --- arrows
 ,("<-","←")
 ,("->","→")
 ,("|->","↦")
 ,("<-|","↤")
 ,("<--","⟵")
 ,("-->","⟶")
 ,("|-->","⟼")
 ,("o->","⇴")
 ,("|^",  "↑")
 ,("|v",  "↓")
 ,("|vv", "↡")
 ,("|^^", "↟")
 ,("||^", "⇑")
 ,("||v", "⇓")
 ,("==>","⟹")
 ,("=>","⇒")
 ,("<=","⇐")
 ,("<=>","⇔")
 ,("|=>","⇨")
 ,("<=|","⇦")
 ,("~>","↝")
 ,("<~","↜")
 ,("~->","⇝")
 ,("<-~","⇜")
 ,("<-<", "↢")
 ,(">->", "↣")
 ,("<->", "↔")
 ,("<|-|>", "⇿")
 ,("-|>", "⇾")
 ,("|<-", "⇤")
 ,("->|", "⇥")
 ,(">>=","↠")
 ,("->>","↠")
 ,("/-", "↼")
 ,("\\-", "↽")
 ,("-/", "⇁")
 ,("-\\", "⇀")
 ,("-|->", "⇸")
 ,("c->", "↪")
 ,("rc->", "↩")
 ,("/v", "↯")
 ,("u^", "↺")
 ,("->->", "⇉")
 ,("2->", "⇉")
 ,("<-<-", "⇇")
 ,("-><-", "⇄")
 ,("<-->", "⇆")
 ,("3->", "⇶")
 ,("-o", "⊸")
 ,("o-o", "⧟")
 {-agda-input arrows
 "l"  "←⇐⇚↤⇦↞↼↽⇠⇺↜⇽⟵⟸↚⇍⇷ ↹     ↢↩↫⇋⇜⇤⟻⟽⤆↶↺⟲                                    "
 "r"  "→⇒⇛↦⇨↠⇀⇁⇢⇻↝⇾⟶⟹↛⇏⇸ ↴    ↣↪↬⇌⇝⇥⟼⟾⤇↷↻⟳⇰⇴⟴⟿ ➵➸➙➔➛➜➝➞➟➠➡➢➣➤➧➨➩➪➫➬➭➮➯➱➲➳➺➻➼➽➾"
  "u"  "↑⇑⟰⇈⇅↥⇧↟↿↾⇡⇞          ↰↱➦ ⇪⇫⇬⇭⇮⇯                                          "
  "d"  "↓⇓⟱⇊⇵↧⇩↡⇃⇂⇣⇟         ↵↲↳➥ ↯                                               "
  "ud" "↕⇕   ↨⇳                                                                   "
  "lr" "↔⇔         ⇼↭⇿⟷⟺↮⇎⇹                                                       "
  "ul" "↖⇖                        ⇱↸                                              "
  "ur" "↗⇗                                         ➶➹➚                            "
  "dr" "↘⇘                        ⇲                ➴➷➘                            "
  "dl" "↙⇙                                                                        "
-}


 --- relations
 ,("c=","⊆")
 ,("/c=","⊈")
 ,("c","⊂")
 ,("/c","⊄")
 ,("c-","∈")
 ,("/c-","∉")
 ,("c/=","⊊")
 ,("rc=","⊇") -- r for reversed
 ,("rc","⊃") -- r for reversed
 ,("rc-","∋") -- r for reversed
 ,("r/c-","∌") -- r for reversed
 ,("rc/=","⊋") -- r for reversed
 ,(">=","≥")
 ,("=<","≤")
 ,("/>=","≱")
 ,("/=<","≰")
 ,("c[]","⊏")
 ,("rc[]","⊐")
 ,("c[]=","⊑")
 ,("rc[]=","⊒")
 ,("/c[]=","⋢")
 ,("/rc[]=","⋣")
 ,("c[]/=","⋤")
 ,("rc[]/=","⋥")

 ---- equal signs
 ,("=def","≝")
 ,("=?","≟")
 ,("=o","≗")
 ,("==","≡")
 ,("===","≣")
 ,("~~","≈")
 ,("/~~","≉")
 ,("/~=","≇")
 ,("/=~","≇")
 ,("-~~","≊")
 ,("~~-","≊")
 ,("~~~","≋")
 ,("-~","≃")
 --,("~-","≃")
 ,("=~","≅")
 --,("~=","≅")
 ,("~","∼")
 ,("/=","≠")
 ,("/==","≢")
 ,(":=","≔")
 ,("=:","≕")

 -- misc
 ,("_|_","⊥")
 ,("Top","⊤")
 ,("||", "∥")
 ,("l","ℓ") -- same as cl

 ,("::","∷")
 ,(":", "∶")
 ,("r;","⁏")
 ,("0", "∅")
 ,("r8","∞")
 ,("*", "★") -- or "⋆"
 ,("/'l","ƛ")
 ,("d","∂")
 ,("#b","♭") -- music bemol
 ,("#f","♮") -- music flat
 ,("##","♯") -- music #
 ,("Hot","♨")
 ,("Cut","✂")
 ,("Pen","✎")
 ,("Tick","✓")
 ,("da","†") -- da(gger)
 ,("micro","µ") -- different than mu:μ
 ,("os", "§")
 ,("so", "§")
 ,("PP", "¶")
 ,("CCCP", "☭")
 ,("ck", "⌥")
 ,(":)", "☺")
 ,(":(", "☹")
 ,("<3", "♥")
 ,("sp", "♠")
 ,("Sp", "♠")
 ,("di", "♦")
 ,("Di", "♦")
 ,("<>", "♦")
 ,("Cl", "♣")
 ,("d1", "⚀")
 ,("d2", "⚁")
 ,("d3", "⚂")
 ,("d4", "⚃")
 ,("d5", "⚄")
 ,("d6", "⚅")
 ,("tm", "™")
 ,("Ro", "✊") -- rock
 ,("RO", "✊") -- rock
 ,("Pa", "✋") -- paper
 ,("PA", "✋") -- paper
 ,("Sc", "✌") -- scissors
 ,("SC", "✌") -- scissors
 ,("rt", "ʇ") -- r for reversed
 ,("rh", "ɥ") -- r for reversed
 ,("re", "ǝ") -- r for reversed
 ,("ra", "ɐ") -- r for reversed
 ,("oe", "œ")
 ,("OE", "Œ")
 ,("ae", "æ")
 ,("AE", "Æ")

 -- Currency Symbols
 ,("B|","฿")
 --,("ob","ⓑ") <-- conflict
 ,("e=","€") -- alternatives =C =c E= =E =e
 ,("L-","£") -- alternatives -L
 ,("Y=","¥") -- alternatives =Y
 ,("x.","¤") -- currency sign

{-
CE "₠" # EURO-CURRENCY SIGN
C/ "₡" # COLON SIGN
/C "₡" # COLON SIGN
Cr "₢" # CRUZEIRO SIGN
Fr "₣" # FRENCH FRANC SIGN
L= "₤" # LIRA SIGN
=L "₤" # LIRA SIGN
m/ "₥" # MILL SIGN
/m "₥" # MILL SIGN
N= "₦" # NAIRA SIGN
=N "₦" # NAIRA SIGN
Pt "₧" # PESETA SIGN
Rs "₨" # RUPEE SIGN
W= "₩" # WON SIGN
=W "₩" # WON SIGN
   "₪" # NEW SHEQEL SIGN
d- "₫" # DONG SIGN
   "₭" # KIP SIGN
   "₮" # TUGRIK SIGN
   "₯" # DRACHMA SIGN
   "₰" # GERMAN PENNY SIGN
   "₱" # PESO SIGN
   "₲" # GUARANI SIGN
   "₳" # AUSTRAL SIGN
   "₴" # HRYVNIA SIGN
   "₵" # CEDI SIGN
|c "¢" # CENT SIGN
c| "¢" # CENT SIGN
c/ "¢" # CENT SIGN
/c "¢" # CENT SIGN
-}

 -- dashes
 ,("-","−")

 -- quotes
 ,("\"","“”")
 ,("r`","′")

 -- turnstyles
 ,("|-", "⊢")
 ,("|/-", "⊬")
 ,("-|", "⊣")
 ,("|=", "⊨")
 ,("|/=", "⊭")
 ,("||-", "⊩")
 ,("|||-", "⊪")

 -- squared operators
 ,("[+]","⊞")
 ,("[-]","⊟")
 ,("[x]","⊠")
 ,("[.]","⊡")
 ,("[]","∎")
 ,("[ ]","☐")
 ,("[Tick]","☑")

 ,("!!","‼")
 ,("??","⁇")
 ,("?!","⁈")
 ,("?b!","‽") -- 'b' like backspace
 ,("!?","⁉")
 ,("r?", "¿") -- 'r' for reversed
 ,("r!", "¡") -- 'r' for reversed

 ,("eth","ð")
 ,("/o","ø")
 ,("/O","Ø")

 ] ++ [ (leading:l, [u]) | leading <- ['|','b'], (l,u) <-

 [("E",'𝔼')
 ,("N",'ℕ')
 ,("H",'ℍ')
 ,("P",'ℙ')
 ,("R",'ℝ')
 ,("C",'ℂ')
 ,("D",'ⅅ')
 ,("Q",'ℚ')
 ,("Z",'ℤ')
 ,("0",'𝟘')
 ,("1",'𝟙')
 ,("2",'𝟚')
 ,("3",'𝟛')
 ,("4",'𝟜')
 ,("5",'𝟝')
 ,("6",'𝟞')
 ,("7",'𝟟')
 ,("8",'𝟠')
 ,("9",'𝟡')
 ,("gg",'ℽ')
 ,("gG",'ℾ')
 ,("gP",'ℿ')
 ,("gS",'⅀')
 ]

 ] ++ [

  -- c for cal
  ("cP","℘")
 ,("cL","ℒ")
 ,("cR","ℛ")
 ,("cN","𝒩")
 ,("cE","ℰ")
 ,("cF","ℱ")
 ,("cH","ℋ")
 ,("cI","ℐ")
 ,("cM","ℳ")
 ,("ce","ℯ")
 ,("cg","ℊ")
 ,("co","ℴ")
 ,("cl","ℓ")
 ] ++ [

  -- r for reversed
  ("rG","⅁")
 ,("r&","⅋")
 ]

circledChars :: [(String,String)]
circledChars =
  -- circles  ⨀"◎◍◐◑◒◓◔◕◖◗◠◡◴◵◶◷⚆⚇⚈⚉"

  [("oo","°")
  ,("o^","°")
  ,("ob","●")
  ,("op","∙")
  ,("ow","○")
  ,("ov","⎉")
  ,("o..","◌")
  ,("oO","◯")
  ,("o+","⊕")
  ,("ox","⊗")

  -- circled operators
  -- ⊝ ⍟ ⎊
  ,n "+"  "⊕"
  ,n "-"  "⊖"
  ,n "/"  "⊘"
  ,n "*"  "⊛"
  ,n "="  "⊜"
  ,n "."  "⊙"
  ,n "()" "⊚"
  ,n "0"  "⓪"
  ,n "1"  "①"
  ,n "2"  "②"
  ,n "3"  "③"
  ,n "4"  "④"
  ,n "5"  "⑤"
  ,n "6"  "⑥"
  ,n "7"  "⑦"
  ,n "8"  "⑧"
  ,n "9"  "⑨"
  ,n "10" "⑩"
  ,n "11" "⑪"
  ,n "12" "⑫"
  ,n "13" "⑬"
  ,n "14" "⑭"
  ,n "15" "⑮"
  ,n "16" "⑯"
  ,n "17" "⑰"
  ,n "18" "⑱"
  ,n "19" "⑲"
  ,n "20" "⑳"
  ,n "A"  "Ⓐ"
  ,n "B"  "Ⓑ"
  ,n "C"  "Ⓒ"
  ,n "D"  "Ⓓ"
  ,n "E"  "Ⓔ"
  ,n "F"  "Ⓕ"
  ,n "G"  "Ⓖ"
  ,n "H"  "Ⓗ"
  ,n "I"  "Ⓘ"
  ,n "J"  "Ⓙ"
  ,n "K"  "Ⓚ"
  ,n "L"  "Ⓛ"
  ,n "M"  "Ⓜ"
  ,n "N"  "Ⓝ"
  ,n "O"  "Ⓞ"
  ,n "P"  "Ⓟ"
  ,n "Q"  "Ⓠ"
  ,n "R"  "Ⓡ"
  ,n "S"  "Ⓢ"
  ,n "T"  "Ⓣ"
  ,n "U"  "Ⓤ"
  ,n "V"  "Ⓥ"
  ,n "W"  "Ⓦ"
  ,n "X"  "Ⓧ"
  ,n "Y"  "Ⓨ"
  ,n "Z"  "Ⓩ"
  ,n "a"  "ⓐ"
  ,n "b"  "ⓑ"
  ,n "c"  "ⓒ"
  ,n "d"  "ⓓ"
  ,n "e"  "ⓔ"
  ,n "f"  "ⓕ"
  ,n "g"  "ⓖ"
  ,n "h"  "ⓗ"
  ,n "i"  "ⓘ"
  ,n "j"  "ⓙ"
  ,n "k"  "ⓚ"
  ,n "l"  "ⓛ"
  ,n "m"  "ⓜ"
  ,n "n"  "ⓝ"
  ,n "o"  "ⓞ"
  ,n "p"  "ⓟ"
  ,n "q"  "ⓠ"
  ,n "r"  "ⓡ"
  ,n "s"  "ⓢ"
  ,n "t"  "ⓣ"
  ,n "u"  "ⓤ"
  ,n "v"  "ⓥ"
  ,n "w"  "ⓦ"
  ,n "x"  "ⓧ"
  ,n "y"  "ⓨ"
  ,n "z"  "ⓩ"
  ]
  where n x y = ("(" ++ x ++ ")", y)

checkAmbs :: [(String, String)] -> [(String, String)]
checkAmbs table = check
  where ambs = [ (x, y)
               | v@(x, _) <- table
               , w@(y, _) <- table
               , v /= w
               , x `isPrefixOf` y ]
        check | null ambs = table
              | otherwise = error $ "checkAmbs: ambiguous declarations for " ++ show ambs

disamb :: [(String, String)] -> [(String, String)]
disamb table = concatMap f table
  where f v@(x, vx) =
            let ambs = [ w
                       | w@(y, _) <- table
                       , v /= w
                       , x `isPrefixOf` y ]
            in if null ambs then [v] else [(x ++ " ", vx), (x ++ "\t", vx)]


-- More:
-- arrows: ⇆
-- turnstyles: ⊦ ⊧
-- subscript: ₔ

zipscripts :: Char -> String -> String -> [(String, String)]
zipscripts c ascii unicode
  = zip (fmap ((c:) . pure) ascii) (fmap pure unicode)

subscripts, superscripts :: [(String, String)]

subscripts   = zipscripts '_' ("0123456789+-=()aeioruvx"++"hklmnpst")
                              ("₀₁₂₃₄₅₆₇₈₉₊₋₌₍₎ₐₑᵢₒᵣᵤᵥₓ"++ hklmnpst)
  where hklmnpst = "\8341\8342\8343\8344\8345\8346\8347\8348"
  -- "ₕₖₗₘₙₚₛₜ" http://hackage.haskell.org/trac/ghc/ticket/5519

superscripts = zipscripts '^' -- NOTE that qCFQSVXYZ are missing
  "0123456789+-=()abcdefghijklmnoprstuvwxyzABDEGHIJKLMNOPRTUW"
  "⁰¹²³⁴⁵⁶⁷⁸⁹⁺⁻⁼⁽⁾ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒᵖʳˢᵗᵘᵛʷˣʸᶻᴬᴮᴰᴱᴳᴴᴵᴶᴷᴸᴹᴺᴼᴾᴿᵀᵁᵂ"
  -- is '\7471' a superscript C ?

{-
 ∣ ⋆ ⁺  ⁻ • ✶ ≺ ″ ≳ ≲ ◁ ∗ ≰ ‿ ⊴ ≮ □ ⇛  ≯
 ↛ ⋐ ⁆ ⁅ ϕ ◂ ≴ ≁ ⑵ ⑴  ̂ ≻ ► ∔ ▶ ≛ ⦈ ⦇ ⑶ ⋃ ⋂ ≵ ½ ’ —
 ∁ Μ ı

 ‿
-}
