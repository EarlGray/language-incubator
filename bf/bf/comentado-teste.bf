
0           1                 2                1
 ++++++++++ >++++++++++++++++[>++++++++++++++++<-]-

Agora p(0) cont�m 10 (ASCII LF) e p(2) cont�m 255 (16 * 16 menos 1); 
p(1) � 0; o ponteiro
atual � p(1): Agora; movamos p(2) para p(1): O loop anterior  multiplica o
ponteiro p(2) por 16 (soma 16 (que est� contido no ponteiro p(1)) a 
p(2) 16
vezes): Isso faz que nosso programa seja independente da 
implementa��o
(portabilidade em BrainFuck!!!!):

2 1 2
>[<+>-]

Movendo o conte�do de p(2) oara p(1)

Tudo preparado: p(2) cont�m 0 e p(1) cont�m 255: O ponteiro atual 
� p(2):
Podemos come�ar a imprimir:

1 2  10 121
<[>+.<<.>><-]
Fazendo um loop; usando p(1) como contador:

