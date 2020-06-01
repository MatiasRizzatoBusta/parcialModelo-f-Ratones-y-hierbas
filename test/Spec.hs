import Test.Hspec
import Lib

comunidad = [cerebro,bicenterra,huesudo]

main :: IO()
main = hspec $ do

   describe "Aplico Cada hierba a un raton" $ do
      it "Cerebro come HierbaBuena" $ do
       hierbaBuena cerebro `shouldBe` cerebro{edad = 3.0}
      it "Cerebro come HierbaVerde para sacar enfermedades con terminacion sis " $ do
       hierbaVerde "sis" cerebro `shouldBe` cerebro{enfermedades = ["sarampion"]}
      it "Cerebro come alcachofa" $ do
       alcachofa cerebro `shouldBe` cerebro{peso=0.19}
      it "Cerebro come hierbaZort" $ do
       hierbaZort cerebro `shouldBe` cerebro{nombre="Pinky",edad = 0,enfermedades =[]}
      it "Cerebro come HierbaDelDiablo" $ do
       hierbaDelDiablo cerebro `shouldBe` cerebro{peso=0.1,enfermedades=["tuberculosis"]}
      
   describe "Aplico medicamentos a un raton" $ do
      it "administramos pondsAntiAge al ratón Bicenterrata " $ do
       pondsAntiAge bicenterra `shouldBe` bicenterra{edad=2.0,peso=0.19}
      --it "administramos reduceFatFast con potencia 1 a huesudo" $ do
      -- reduceFatFast huesudo `shouldBe` huesudo{peso=9,enfermedades=["sinusitis"]}
      it "aplico pdepCIlina a cerebro" $ do
       pdepCilina cerebro `shouldBe` cerebro{enfermedades=["sarampion"]}
   
   describe "Veo Si un medicamento estabiliza una comunidad" $ do
      it "aplico pondsAntiAge a una comunidad compuesta por bicenterrra" $ do
       [bicenterra] `shouldSatisfy` (lograEstabilizar pondsAntiAge)
      it "aplico pdepCilina a comunidad" $ do
       comunidad `shouldNotSatisfy` (lograEstabilizar pdepCilina)