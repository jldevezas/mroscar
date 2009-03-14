%
% OSCARES 2008
% Prémios da Academia de Artes e Ciências Cinematográficas
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

% Validações

verifica_sujeito(Sujeito):-
    filme(Sujeito);
    actor(Sujeito);
    realizador(Sujeito).

% Converter lista em resultados individuais

converte_em_resultados([]).

converte_em_resultados([H|T]):-
    assert(resultado(H)),
    converte_em_resultados(T).

% PROGRAMA PRINCIPAL

ln(Frase):-
    transf_lista(Frase,LPal),
	( verifica_frase(LPal,[]);
		( erro(semantico),write('Erro semântico!'),assert(resultado('Erro semântico'));
		  (write('Erro sintático!'),assert(resultado('Erro sintático')))
		)
	).

verifica_frase -->
    verifica_frase_afirmativa;
    verifica_frase_interrogativa.

verifica_frase_interrogativa -->
    pron_int,
    verbo(N,_,ser),
    nome(_-N,Accao),
    sintagma_prep(Objecto),
    {resposta_interrogacao(Accao,Objecto)}.

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
    write('Não'),assert(resultado('Não')).

resposta_interrogacao(Accao,Objecto):-
    (Facto=..[Accao,Sujeito,Objecto,Nome],
    findall(Nome,Facto,Resultados),
    write(Resultados),converte_em_resultados(Resultados));
    write('Sem Resultados'),assert(resultado('Sem Resultados')).

% GRAMÁTICA

det(m-s) --> ['O'];[o].
det(m-p) --> ['Os'];[os].
det(f-s) --> ['A'];[a].
det(f-p) --> ['As'];[as].

prep(m-s) --> ['Ao'];[ao].
prep(f-s) --> ['À'];[à].
prep(_) --> ['De'];[de].
prep(_) --> ['Para'];[para].
pron_int --> ['Quem'];['quem'].

verbo(s,Sujeito,ganhar) --> [ganhou],{verifica_sujeito(Sujeito);assert(erro(semantico)),fail}.
verbo(s,_,ser) --> [foi].

% Filmes
nome(m-s,thedarkknight) --> ['The','Dark','Knight'].
nome(m-s,nocountryforoldmen) --> ['No','Country',for,'Old','Men'].

% Actores
nome(m-s, danieldaylewis) --> ['Daniel', 'Day-Lewis'].

% Realizadores
nome(p-s, irmaoscoen) --> [irmãos, 'Coen'];['Irmãos', 'Coen'].

% Prémios
nome(m-s,melhorfilme) --> [melhor,filme].
nome(m-s,melhorrealizador) --> [melhor,realizador].
nome(m-s,melhoractor) --> [melhor,actor].
nome(f-s,melhoractriz) --> [melhor,actriz].
nome(m-s,melhoractorsec) --> [melhor,actor,secundário].
nome(f-s,melhoractrizsec) --> [melhor,actriz,secundária].
nome(m-s,melhorargorig) --> [melhor,argumento,original].
nome(m-s,melhorargadapt) --> [melhor,argumento,adaptado].
nome(m-s,melhorfilmeanim) --> [melhor,filme,de,animação].
nome(m-s,melhorfilmelingestr) --> [melhor,filme,em,lingua,estrangeira].
nome(f-s,melhorfotografia) --> [melhor,fotografia].
nome(f-s,melhormisturasom) --> [melhor,mistura,de,som].

% Vocabulário geral
nome(m-s,premio) --> [prémio].
nome(m-p,filme) --> [filmes].
nome(m-s,nomeado) --> [nomeado].

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

% Prémios
ganhar(nocountryforoldmen,melhorfilme,'No Country for Old Men').
ganhar(danieldaylewis,melhorrealizador,'Daniel Day-Lewis').
ganhar(thedarkknight,melhormisturasom,'The Dark Knight').

% Nomeados
nomeado(atonement,melhorfilme,'Atonement').
nomeado(juno,melhorfilme,'Juno').
nomeado(michaelclayton,melhorfilme,'Michael Clayton').
nomeado(therewillbeblood,melhorfilme,'There Will Be Blood').
nomeado(nocountryforoldmen,melhorfilme,'No Country for Old Men').