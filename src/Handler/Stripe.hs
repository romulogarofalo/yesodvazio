{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Stripe where

import Import as I
import Handler.Funcs as F
import Web.Stripe
import Web.Stripe.Customer
import Web.Stripe.Charge
import Web.Stripe.StripeRequest as S

optionsPaymentWithStripeR :: Handler ()
optionsPaymentWithStripeR = anyOriginIn [ F.OPTIONS, F.POST ]


-- data CreditCardJson = CreditCardJson { 
--     cardNumber :: Text,
--     expMonth   :: Int,                              
--     expYear    :: Int,
--     cvc        :: Text
--     } deriving (Show, Eq, Ord)


-- instance ToJSON CreditCardJson where
--     toJSON (CreditCardJson cardNumber expMonth expYear cvc) =
--         I.object ["cardNumber" .= cardNumber, "expMonth" .= expMonth, "expYear" .= expYear, "cvc" .= cvc]

-- instance FromJSON CreditCardJson where
--     parseJSON (Object v) = CreditCardJson <$>
--                             v .: "cardNumber" <*>
--                             v .: "expMonth" <*>
--                             v .: "expYear" <*>
--                             v .: "cvc"
--     parseJSON _ = empty

-- '{"cardNumber":"4242424242424242", "expMonth":10, "expYear":2020, "cvc":"312"}'

postPaymentWithStripeR :: Handler Value
postPaymentWithStripeR = do
    (cardNumber, expMonth, expYear, cvc) <- requireJsonBody :: Handler (Text, Int, Int,Text)
    let config = StripeConfig (StripeKey "sk_test_")
        credit = CardNumber "4242424242424242"
        em  = ExpMonth 12
        ey  = ExpYear 2020
        cvc = CVC "314"
        aaa = Just cvc
        cardinfo = (NewCard credit em ey aaa)
    result <- liftIO $ stripe config (createCustomer S.-&- cardinfo)
    case result of
        (Left  stripeError) -> sendStatusJSON ok200 $ I.object [ "resp" .= ((errorMsg)stripeError)]
        (Right customer)    -> sendStatusJSON ok200 $ I.object [ "resp" .= ("boa"::Text)]






-- postPayWithCreditStripeR :: Handler Value
-- postPayWithCreditStripeR = do

-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "sk_test_FMaMv6zghjzhNXDrgq5sTOTk")
--       credit = CardNumber "4242424242424242"
--       em  = ExpMonth 12
--       ey  = ExpYear 2015
--       cvc = CVC "123"
--       cardinfo = (newCard credit em ey) { newCardCVC = Just cvc }
--   result <- stripe config createCustomer
--                              -&- cardinfo
--   case result of
--     (Left stripeError) -> print stripeError
--     (Customer { customerId = cid }) ->
--       do result <- stripe config $ createCharge (Amount 100) USD
--                                      -&- cid
--          case result of
--            Left  stripeError -> print stripeError
--            Right charge      -> print charge