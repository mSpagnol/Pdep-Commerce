type Producto = (String, Float)

cocaCola :: Producto
cocaCola = ("Coca Cola", 150.00)

nombreDelProducto :: Producto -> String
nombreDelProducto (unNombre, _) = unNombre

precio :: Producto -> Float
precio (_, unPrecio) = unPrecio

productoCodiciado :: Producto -> Bool
productoCodiciado unProducto = (>10) . largoNombreDelProducto $ unProducto

largoNombreDelProducto :: Producto -> Int
largoNombreDelProducto unProducto = length . nombreDelProducto $ unProducto

productoCorriente :: Producto -> Bool
productoCorriente unProducto = esVocal . inicialNombreProducto $ unProducto

inicialNombreProducto :: Producto -> Char
inicialNombreProducto unProducto = head . nombreDelProducto $ unProducto

esVocal :: Char -> Bool 
esVocal unCaracter = elem unCaracter "aeiouAEIOU"

productoXL :: Producto -> String
productoXL unProducto = (++ "XL") . nombreDelProducto $ unProducto

aplicarCostoDeEnvio :: Producto -> Float -> Float
aplicarCostoDeEnvio unProducto costoDeEnvio = precio unProducto + costoDeEnvio

productoDeLujo :: Producto -> Bool
productoDeLujo unProducto = ((elem 'x') . nombreDelProducto  $ unProducto) || ((elem 'z') . nombreDelProducto $ unProducto)

aplicarDescuento :: Producto -> Float -> Float
aplicarDescuento unProducto descuento = precio unProducto - descuento

productoDeElite :: Producto -> Bool
productoDeElite unProducto = productoCodiciado unProducto && productoDeLujo unProducto && not (productoCorriente unProducto) 

precioTotal :: Producto -> Float -> Float -> Float -> Float
precioTotal unProducto cantidad descuento costoDeEnvio = (+) (aplicarDescuentoEnCantidad unProducto cantidad descuento) (costoDeEnvio)

aplicarDescuentoEnCantidad :: Producto -> Float -> Float -> Float
aplicarDescuentoEnCantidad unProducto cantidad descuento = (*cantidad) . (aplicarDescuento unProducto) $ descuento

entregaSencilla :: String -> Bool
entregaSencilla diaDeEntrega = even . length $ diaDeEntrega

descodiciarProducto :: Producto -> String
descodiciarProducto unProducto = (take 10) . nombreDelProducto $ unProducto

versionBarata :: Producto -> String
versionBarata unProducto = reverse . descodiciarProducto $ unProducto
