frecuenciaCardiacaPromedio :: Int
frecuenciaCardiacaPromedio = 80

hacerActividadFisica :: Int -> Int --un solo parametro porque recibe frecuencia que le vamos a dar
hacerActividadFisica unaFrecuencia = unaFrecuencia + 50

tieneTaquicardia :: Int -> Bool
tieneTaquicardia unaFrecuencia = unaFrecuencia >=180

tieneTaquicardiaDespuesDeEntrenar2Veces :: Int -> Bool
tieneTaquicardiaDespuesDeEntrenar2Veces unaFrecuencia = 
    (tieneTaquicardia . hacerActividadFisica . hacerActividadFisica) unaFrecuencia

suma :: Num a => a -> a -> a
suma unNumero otroNumero = unNumero + otroNumero