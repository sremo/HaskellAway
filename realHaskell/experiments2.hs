-- read file with historical prices
-- test for reversal


-- recursion plot compute

-- implement qq plot http://en.wikipedia.org/wiki/Q-Q_plot

-- take a list (time serie), a value (threshold distance)
--recursionPlot:: (Ord a, Num a) => [a] -> a -> Array a
--recursionPlot (x:xs) d =

data OrderType = Buy | Sell

type Price = Int
data OptionDirection = Call | Put
data OptionType = European | American | Asian
type StrikePrice = Int
type Maturity = Int
type OptionUnderlying = Asset
--data Option = Option OptionDirection OptionType OptionUnderlying StrikePrice Maturity

type Ticker = String

--data Stock = Stock Ticker

type Issuer = String


--data Bond = Bond Issuer


data Asset =      Stock Ticker
		| Bond Issuer
		| Option OptionDirection OptionType (Stock Ticker)
		deriving Show
--data Asset = Stock | Bond | Option


instance Show OrderType where
	show Buy = "Buy"
	show Sell = "Sell"

instance Show OptionType where
	show European = "European"
	show American = "American"
	show Asian = "Asian"
	show _ = "Other"

instance Show OptionDirection where
	show Call = "Call"
	show Put = "Put"
