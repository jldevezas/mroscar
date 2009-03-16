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

% Converter lista em resultados individuais

converte_em_resultados([]).

converte_em_resultados([H|T]):-
    assert(resultado(H)),
    converte_em_resultados(T).

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
    write('N�o'),assert(resultado('N�o')).

resposta_interrogacao(Accao,Objecto):-
    (Facto=..[Accao,Sujeito,Objecto,Nome],
    findall(Nome,Facto,Resultados),
    write(Resultados),converte_em_resultados(Resultados));
    write('Sem Resultados'),assert(resultado('Sem Resultados')).

% GRAM�TICA

det(m-s) --> ['O'];[o].
det(m-p) --> ['Os'];[os].
det(f-s) --> ['A'];[a].
det(f-p) --> ['As'];[as].

prep(m-s) --> ['Ao'];[ao].
prep(f-s) --> ['�'];[�].
prep(_) --> ['De'];[de].
prep(_) --> ['Para'];[para].
pron_int --> ['Quem'];['quem'].

verbo(s,Sujeito,ganhar) --> [ganhou],{verifica_sujeito(Sujeito);assert(erro(semantico)),fail}.
verbo(s,_,ser) --> [foi].

% Filmes
nome(m-s,'The Dark Knight') --> ['The','Dark','Knight'].
nome(m-s,'No Country for Old Men') --> ['No','Country',for,'Old','Men'].
nome(f-s,'The Golden Compass') --> ['The','Gold','Compass'].
nome(m-s,'There Will Be Blood') --> ['There','Will','Be','Blood'].
nome(m-s,'Michael Clayton') --> ['Michael','Clayton'].
nome(m-s,'Juno') --> ['Juno'].

% Actores
nome(m-s, 'Daniel Day-Lewis') --> ['Daniel', 'Day-Lewis'].

% Realizadores
nome(p-s, 'Irm�os Coen') --> [irm�os, 'Coen'];['Irm�os', 'Coen'].

% Pr�mios
nome(m-s,'Melhor Filme') --> [melhor,filme].
nome(m-s,'Melhor Realizador') --> [melhor,realizador].
nome(m-s,'Melhor Actor') --> [melhor,actor].
nome(f-s,'Melhor Actriz') --> [melhor,actriz].
nome(m-s,'Melhor Actor Secund�rio') --> [melhor,actor,secund�rio].
nome(f-s,'Melhor Actriz Secund�ria') --> [melhor,actriz,secund�ria].
nome(m-s,'Melhor Argumento Original') --> [melhor,argumento,original].
nome(m-s,'Melhor Argumento Adaptado') --> [melhor,argumento,adaptado].
nome(m-s,'Melhor Filme de Anima��o') --> [melhor,filme,de,anima��o].
nome(m-s,'Melhor Filme em Lingua Estrangeira') --> [melhor,filme,em,lingua,estrangeira].
nome(f-s,'Melhor Fotografia') --> [melhor,fotografia].
nome(f-s,'Melhor Direc��o Art�stica') --> [melhor,direc��o,art�stica].
nome(m-s,'Melhor Guarda-Roupa') --> [melhor,guarda-roupa].
nome(f-s,'Melhor Edi��o') --> [melhor,edi��o].
nome(f-s,'Melhor Caracteriza��o') --> [melhor,caracteriza��o].
nome(m-p,'Melhores Efeitos Visuais') --> [melhores,efeitos,visuais].
nome(f-s,'Melhor Montagem de Som') --> [melhor,montagem,de,som].
nome(f-s,'Melhor Mistura de Som') --> [melhor,mistura,de,som].
nome(f-s,'Melhor Banda Sonora') --> [melhor,banda,sonora].
nome(f-s,'Melhor Can��o Original') --> [melhor,can��o,original].
nome(m-s,'Melhor Document�rio') --> [melhor,document�rio].
nome(m-s,'Melhor Document�rio em Curta-Metragem') --> [melhor,document�rio,em,curta-metragem].
nome(f-s,'Melhor Curta Metragem') --> [melhor,curta-metragem].
nome(f-s,'Melhor Curta Metragem de Anima��o') --> [melhor,curta-metragem,de,anima��o].
nome(m-s,'�scar Honor�rio) --> [�scar,honor�rio].

% Vocabul�rio geral
nome(m-s,premio) --> [pr�mio].
nome(m-p,filme) --> [filmes].
nome(m-s,nomeado) --> [nomeado].

% BASE DE CONHECIMENTO

% Filmes
filme('The Dark Knight').
filme('Juno').
filme('Michael Clayton').
filme('There Will Be Blood').
filme('No Country for Old Men').
filme('The Golden Compass').

% Actores
actor('Daniel Day-Lewis').

% Realizadores
realizador('Irm�os Coen').

% Pr�mios
ganhar(nocountryforoldmen,'Melhor Filme','No Country for Old Men').
ganhar(danieldaylewis,'Melhor Realizador','Daniel Day-Lewis').
ganhar(thedarkknight,'Melhor Mistura de Som','The Dark Knight').
ganhar(thegoldencompass,'Melhores Efeitos Visuais','The Golden Compass').

% Nomeados

nomeado(atonement,'Melhor Filme','Atonement').
nomeado(juno,'Melhor Filme','Juno').
nomeado(michaelclayton,'Melhor Filme','Michael Clayton').
nomeado(therewillbeblood,'Melhor Filme','There Will Be Blood').
nomeado(nocountryforoldmen,'Melhor Filme','No Country for Old Men').

nomeado(thegoldencompass,'Melhores Efeitos Visuais','The Golden Compass').