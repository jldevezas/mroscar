%
% OSCARES 2008
% Pr�mios da Academia de Artes e Ci�ncias Cinematogr�ficas
%
% Linguagem Natural
%
% ln('<frase>').
%

:-abolish(resultado/1).
:-abolish(erro/1).

:-dynamic erro/1.
:-dynamic resultado/1.

% PREDICADOS AUXILIARES

% Transformar frase em lista de palavras

transf_lista(Frase,LPal):-
    string_to_list(Frase,LChar),
    faz_palavras(LChar,[],LPal),!.

faz_palavras([],LChar,[Pal]):-atom_chars(Pal,LChar).

faz_palavras([32|T],LCharAux,[HPal|TPal]):-
    atom_chars(HPal,LCharAux),
    faz_palavras(T,[],TPal).

faz_palavras([H|T],LChar,LPal):-
    append(LChar,[H],LCharAux),
    faz_palavras(T,LCharAux,LPal).

% Valida��es

verifica_sujeito(Sujeito):-
    filme(Sujeito);
    actor(Sujeito);
    realizador(Sujeito).

% PROGRAMA PRINCIPAL

ln(Frase):-
    transf_lista(Frase,LPal),
	( verifica_frase_afirmativa(LPal,[]);
		( erro(semantico),write('Erro sem�ntico!'),
		    assert(resultado('Erro sem�ntico'));
		  (write('Erro sint�tico!'),assert(resultado('Erro sint�tico')))
		)
	).

verifica_frase_afirmativa -->
    sintagma_nominal(_-N,Sujeito),
    sintagma_verbal(N,Sujeito,Accao,Objecto),
    {resposta(Sujeito,Accao,Objecto)}.

sintagma_nominal(G-N,Sujeito) -->
    nome(G-N,Sujeito).

sintagma_nominal(G-N,Sujeito) -->
    det(G-N),
    nome(G-N,Sujeito).

sintagma_verbal(N,Sujeito,Accao,Objecto) -->
    verbo(N,Sujeito,Accao),
    complemento_directo(N,Objecto).

% Ex.: o pr�mio de melhor mistura de som
complemento_directo(N,Objecto) -->
    det(G-N),
	nome(G-N,_),
    prop(_),
    nome(_-N,Objecto).

% Ex.:a melhor mistura de som
complemento_directo(N,Objecto) -->
    det(G-N),
	nome(G-N,Object).

% RESPOSTAS

resposta(Sujeito,Accao,Objecto):-
    (Facto=..[Accao,Sujeito,Objecto],
    Facto,
    write('Sim'),assert(resultado('Sim')));
    write('N�o'),assert(resultado('N�o')).

% GRAM�TICA

det(m-s) --> ['O'];[o].
det(m-p) --> ['Os'];[os].
det(f-s) --> ['A'];[a].
det(f-p) --> ['As'];[as].

prop(m-s) --> ['Ao'];[ao].
prop(f-s) --> ['�'];[�].
prop(_-_) --> ['de'].

verbo(s,Sujeito,ganhar) --> [ganhou],{verifica_sujeito(Sujeito);assert(erro(semantico)),fail}.

% Filmes
nome(m-s,thedarkknight) --> ['The', 'Dark', 'Knight'].

% Actores
nome(m-s, danieldaylewis) --> ['Daniel', 'Day-Lewis'].

% Realizadores
nome(p-s, irmaoscoen) --> [irm�os, 'Coen'];['Irm�os', 'Coen'].

% Pr�mios
nome(m-s,melhorfilme) --> [melhor, filme];['Melhor', 'Filme'].
nome(m-s,melhorrealizador) --> [melhor, realizador];['Melhor', 'Realizador'].
nome(m-s,melhoractor) --> [melhor, actor];['Melhor', 'Actor'].
nome(f-s,melhoractriz) --> [melhor, actriz];['Melhor', 'Actriz'].
nome(f-s,melhormisturasom) --> [melhor, mistura, de, som];['Melhor', 'Mistura', de, 'Som'].

% Vocabul�rio geral
nome(m-s,premio) --> [pr�mio].

% BASE DE CONHECIMENTO

% Filmes
filme(thedarkknight).

% Actores
actor(danieldaylewis).

% Realizadores
realizador(irmaoscoen).

% Pr�mios
ganhar(thedarkknight,melhormisturasom).
ganhar(danieldaylewis,melhorrealizador).