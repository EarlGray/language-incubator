
0           1                 2                1
 ++++++++++ >++++++++++++++++[>++++++++++++++++<-]-

Agora p(0) contém 10 (ASCII LF) e p(2) contém 255 (16 * 16 menos 1); 
p(1) é 0; o ponteiro
atual é p(1): Agora; movamos p(2) para p(1): O loop anterior  multiplica o
ponteiro p(2) por 16 (soma 16 (que está contido no ponteiro p(1)) a 
p(2) 16
vezes): Isso faz que nosso programa seja independente da 
implementação
(portabilidade em BrainFuck!!!!):

2 1 2
>[<+>-]

Movendo o conteúdo de p(2) oara p(1)

Tudo preparado: p(2) contém 0 e p(1) contém 255: O ponteiro atual 
é p(2):
Podemos começar a imprimir:

1 2  10 121
<[>+.<<.>><-]
Fazendo um loop; usando p(1) como contador:

