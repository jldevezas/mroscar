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
	( verifica_frase(LPal,[]);
		( erro(semantico),write('Erro sem�ntico!'),assert(resultado('Erro sem�ntico'));
		  (write('Erro sint�tico!'),assert(resultado('Erro sint�tico')))
		)
	).

verifica_frase -->
    verifica_frase_afirmativa;
    verifica_frase_interrogativa.

verifica_frase_interrogativa -->
    pron_int,
    sintagma_verbal(_,Sujeito,Accao,Objecto),
    {resposta_interrogacao(Accao,Objecto)}.


verifica_frase_afirmativa -->
    sintagma_nominal(_-N,Sujeito),
    sintagma_verbal(N,Sujeito,Accao,Objecto),
    {resposta(Sujeito,Accao,Objecto)}.

sintagma_nominal(G-N,Sujeito) -->
    det(G-N),
    nome(G-N,Sujeito).

sintagma_nominal(G-N,Sujeito) -->
    nome(G-N,Sujeito).

sintagma_verbal(N,Sujeito,Accao,Objecto) -->
    verbo(N,Sujeito,Accao),
    sintagma_nominal(_,_),
    sintagma_prep(Objecto).

sintagma_verbal(N,Sujeito,Accao,Objecto) -->
    verbo(N,Sujeito,Accao),
    sintagma_nominal(_,Objecto).

sintagma_prep(Objecto) -->
    prep(G-N),
    sintagma_nominal(G-N,Objecto).

% RESPOSTAS

resposta(Sujeito,Accao,Objecto):-
    (Facto=..[Accao,Sujeito,Objecto,_],
    Facto,
    write('Sim'),assert(resultado('Sim')));
    write('N�o'),assert(resultado('N�o')).

resposta_interrogacao(Accao,Objecto):-
    (Facto=..[Accao,Sujeito,Objecto,Nome],
    Facto,
    write(Nome),assert(resultado(Nome)));
    write('Sem Resultados'),assert(resultado('Sem Resultados')).


% GRAM�TICA

det(m-s) --> ['O'];[o].
det(m-p) --> ['Os'];[os].
det(f-s) --> ['A'];[a].
det(f-p) --> ['As'];[as].

prep(m-s) --> ['Ao'];[ao].
prep(f-s) --> ['�'];[�].
prep(_) --> ['De'];[de].

pron_int --> ['Quem'];['quem'].

verbo(s,Sujeito,ganhar) --> [ganhou],{verifica_sujeito(Sujeito);assert(erro(semantico)),fail}.

% Filmes
nome(m-s,thedarkknight) --> ['The','Dark','Knight'].
nome(m-s,nocountryforoldmen) --> ['No','Country',for,'Old','Men'].

% Actores
nome(m-s, danieldaylewis) --> ['Daniel', 'Day-Lewis'].

% Realizadores
nome(p-s, irmaoscoen) --> [irm�os, 'Coen'];['Irm�os', 'Coen'].

% Pr�mios
nome(m-s,melhorfilme) --> [melhor,filme].
nome(m-s,melhorrealizador) --> [melhor,realizador].
nome(m-s,melhoractor) --> [melhor,actor].
nome(f-s,melhoractriz) --> [melhor,actriz].
nome(m-s,melhoractorsec) --> [melhor,actor,secund�rio].
nome(f-s,melhoractrizsec) --> [melhor,actriz,secund�ria].
nome(m-s,melhorargorig) --> [melhor,argumento,original].
nome(m-s,melhorargadapt) --> [melhor,argumento,adaptado].
nome(m-s,melhorfilmeanim) --> [melhor,filme,de,anima��o].
nome(m-s,melhorfilmelingestr) --> [melhor,filme,em,lingua,estrangeira].
nome(f-s,melhorfotografia) --> [melhor,fotografia].
nome(f-s,melhormisturasom) --> [melhor,mistura,de,som].

% Vocabul�rio geral
nome(m-s,premio) --> [pr�mio].

% BASE DE CONHECIMENTO

% Filmes
filme(thedarkknight).
filme(juno).
filme(michaelclayton).
filme(therewillbeblood).
filme(nocountryforoldmen).

% Actores
actor(danieldaylewis).

% Realizadores
realizador(irmaoscoen).

% Pr�mios
ganhar(nocountryforoldmen,melhorfilme,'No Country for Old Men').
ganhar(danieldaylewis,melhorrealizador,'Daniel Day-Lewis').
ganhar(thedarkknight,melhormisturasom,'The Dark Knight').

% Nomeados
nomeado(atonement,melhorfilme).
nomeado(juno,melhorfilme).
nomeado(michaelclayton,melhorfilme).
nomeado(therewillbeblood,melhorfilme).
nomeado(nocountryforoldmen,melhorfilme).