## caso variancia populacao e media 
function(alfa, sigma, n, x_barra) {
    z = qnorm(1 - alfa/2)
    erro = z * sigma / sqrt(n)
    L = x_barra - erro
    R = x_barra + erro
}

## caso com a amostra
function(x, conf_level) {
    res = t.test(x, conf = conf_level)
    L = res$conf.int[1]
    R = res$conf.int[2]
    print(L)
    print(R)
}

## apenas com média e dp amostrais
function(S, x_barra, alfa, n) {
    t = abs(qt(alfa/2, df = n - 1))
    
    erro = t*S/sqrt(n)
    L = x_barra - erro
    R = x_barra + erro
}

## proporcao
function(alfa, p, n) {
    # p seria a proporcao amostral e pi a propocao pop.
    z = qnorm(1 - alfa/2)
    erro = z * sqrt(p * ( 1- p ) / n)
    ## limites para pi
    L = p - erro
    R = p + erro
}