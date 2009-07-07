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
:-abolish(contexto/2).

:-dynamic erro/1.
:-dynamic resultado/1.
:-dynamic contexto/2.

% PREDICADOS AUXILIARES

% Transformar frase em lista de palavras

transf_lista(Frase,LPal):-
    string_to_list(Frase,LChar),
    faz_palavras(LChar,[],LPal).

faz_palavras([],LChar,[Pal]):-atom_chars(Pal,LChar).

faz_palavras([32|T],LCharAux,[HPal|TPal]):-
    atom_chars(HPal,LCharAux),
    faz_palavras(T,[],TPal).

faz_palavras([H|T],LChar,LPal):-
    append(LChar,[H],LCharAux),
    faz_palavras(T,LCharAux,LPal).

% Validações

valida_sujeito(Sujeito):-
    filme(Sujeito),!;
    actor(Sujeito,_),!;
    realizador(Sujeito).

% Converter lista em resultados individuais

converte_em_resultados([]).

converte_em_resultados([H|T]):-
    assert(resultado(H)),
    converte_em_resultados(T).

% PROGRAMA PRINCIPAL

ln(Frase):-
    transf_lista(Frase,LPal),
    verifica(LPal).

verifica(ListaPal):-
    ( retractall(erro(semantico)) ; true ),
    ( ( verifica_frase(Estrutura,ListaPal,[]),
        write(Estrutura) )
    ; ( erro(semantico), write('Erro semântico'),assert(resultado('Erro semântico')) )
    ; write('Erro sintático'),assert(resultado('Erro sintático')) ).

% Interrogativas
verifica_frase(frase(SI,SV)) -->
    sintagma_interrogativo(SI,Q,_,Accao1,Objecto1),
    { Objecto1==premio;Objecto1==oscar },
    sintagma_verbal(SV,_,_,Accao,Objecto),
    {resposta2(Q,Accao1,Objecto1,Accao,Objecto)}.

verifica_frase(frase(SI,SV)) -->
    sintagma_interrogativo(SI,Q,N,Accao1,Objecto1),
    sintagma_verbal(SV,N,_,Accao,Objecto),
    {resposta(Q,Accao1,Objecto1,Accao,Objecto)}.

% Elipse
verifica_frase(frase(SP)) -->
        ['E'],sintagma_prep(SP,Objecto),
        { contexto(Q,Accao),
          resposta(Q,_,_,Accao,Objecto) }.

verifica_frase(frase(SN)) -->
        ['E'],sintagma_nominal(SN,_,[Objecto]),
        { contexto(Q,Accao),
          resposta2(Q,_,_,Accao,Objecto) }.
          
verifica_frase(frase(SN)) -->
        ['E'],sintagma_nominal(SN,_,[Objecto]),
        { contexto(Q,Accao),
          resposta(Q,_,_,Accao,Objecto) }.

% Afirmativa
verifica_frase(frase(SN,SV)) -->
    sintagma_nominal(SN,N,Sujeito),
    sintagma_verbal(SV,N,Sujeito,Accao,Objecto),
    {concorda(Accao,Sujeito,Objecto)}.
		  
sintagma_interrogativo(pronome_int(I),Q,N,Accao,Objecto) -->
    pron_int(_-N,I,Q),
    sintagma_nominal_int(_,N,Accao,Objecto).

sintagma_interrogativo(pronome_int(I),Q,N,_,_) -->
    pron_int(_-N,I,Q).

sintagma_nominal_int(sintg_nominal(det(D),nome(Nome)),N,_,Objecto) -->
    det(G-N,D),
    nome(G-N,Nome,Objecto).
    
sintagma_nominal_int(sintg_nominal(nome(Nome)),N,_,Objecto) -->
    nome(_-N,Nome,Objecto).

sintagma_prep(sintg_prep(prep(P),SN),Objecto) -->
        prep(_-N,P),
    sintagma_nominal(SN,N,[Objecto]).
        
sintagma_nominal(SN,p,[Sujeito1,Sujeito2]) -->
     sintagma_nominal1(SN1,_,Sujeito1),[e],
     sintagma_nominal1(SN2,_,Sujeito2),
     { SN1=..[_|L1],SN2=..[_|L2],
       append(L1,[e|L2],L),
       SN=..[sintg_nominal|L] }.

sintagma_nominal(SN,N,[Sujeito]) -->
     sintagma_nominal1(SN,N,Sujeito).

sintagma_nominal1(sintg_nominal(det(D),nome(Nome)),N,Sujeito) -->
    det(G-N,D),
    nome(G-N,Nome,Sujeito).

sintagma_nominal1(sintg_nominal(nome(Nome)),N,Sujeito) -->
    nome(_-N,Nome,Sujeito).

sintagma_verbal(sintg_verbal(verbo(V),SN,SP),N,_,Accao,Objecto) -->
        verbo(N,V,_,ser),!,
    sintagma_nominal(SN,N,[Accao]),
        sintagma_prep(SP,Objecto).

sintagma_verbal(sintg_verbal(verbo(V),SN,SP),N,Nome,Accao,Objecto) -->
    verbo(N,V,Nome,Accao),
    sintagma_nominal(SN,N,_),
        sintagma_prep(SP,Objecto).

sintagma_verbal(sintg_verbal(verbo(V),SN),N,Nome,Accao,Objecto) -->
    verbo(N,V,Nome,Accao),
    sintagma_nominal(SN,N,[Objecto]).
    
sintagma_verbal(sintg_verbal(verbo(V),SP),N,Sujeito,Accao,Objecto) -->
    verbo(N,V,Sujeito,Accao),
    sintagma_prep(SP,Objecto).

% RESPOSTAS

% Interrogação

resposta(Q,Accao1,_,Accao,Objecto):-
    ( retractall(contexto(_,_)),assert(contexto(Q,Accao)) ),
        var(Accao1), Predicado=..[Accao, Sujeito,Objecto],
    findall(Sujeito,Predicado,Lista),
    ( ( Q=qual,write(Lista) )
    ; ( length(Lista,Nlista),write(Nlista) )
    ), nl.

resposta(Q,Accao1,Objecto1,Accao,Objecto):-
    nonvar(Accao1),
    Predicado=..[Accao,Sujeito,Objecto],
    Predicado1=..[Accao1,Sujeito,Objecto1],
    findall(Sujeito, ( Predicado, Predicado1 ), Lista),
    ( ( Q=qual,write(Lista) )
    ; ( length(Lista,Nlista),write(Nlista) )
    ), nl.
    
resposta2(Q,Accao1,_,Accao,Objecto):-
        ( retractall(contexto(_,_)),assert(contexto(Q,Accao)) ),
    var(Accao1), Predicado=..[Accao, Objecto,Sujeito],
    findall(Sujeito, Predicado,Lista),
    ( ( Q=qual,write(Lista) )
    ; ( length(Lista,Nlista),write(Nlista) )
    ), nl.

% Afirmação

concorda(Accao,[Sujeito1|OSuj],Objecto):-
    ( Predicado=..[Accao,Sujeito1,Objecto], Predicado,
    concorda(Accao,OSuj,Objecto) )
    ; write('Não'),nl,assert(resultado('Não')).

concorda(Accao,Sujeito,Objecto):-
    ( ( Predicado=..[Accao,Sujeito,Objecto],Predicado,
        write('Sim'),nl,assert(resultado('Sim')) )
    ; write('Não'),nl,assert(resultado('Não')) ).

concorda(_,[],_):- write('Sim'),nl,assert(resultado('Sim')).

% GRAMÁTICA

det(m-s,o) --> ['O'];[o].
det(m-p,os) --> ['Os'];[os].
det(f-s,a) --> ['A'];[a].
det(f-p,as) --> ['As'];[as].

prep(m-s,ao) --> ['Ao'];[ao].
prep(f-s,à) --> ['À'];[à].
prep(_,de) --> ['De'];[de].
prep(_,para) --> ['Para'];[para].
prep(_,em) --> ['Em'];[em].
prep(m-s,no) --> ['No'];[no].
prep(m-p,nos) --> ['Nos'];[nos].
prep(f-s,na) --> ['Na'];[na].
prep(f-p,nas) --> ['Nas'];[nas].
prep(_,por) --> [por].
prep(m-p,pelos) --> [pelos].

pron(_,que) --> [que].

pron_int(_-s,quem,qual) --> ['Quem'];[quem].
pron_int(_-s,qual,qual) --> ['Qual'];[qual].
pron_int(_-p,quais,qual) --> ['Quais'];[quais].
pron_int(_,que,qual) --> ['Que'];[que].
pron_int(m-p,quantos,quant) --> ['Quantos'];[quantos].
pron_int(f-p,quantas,quant) --> ['Quantas'];[quantas].

verbo(s,ganhou,[Sujeito],ganhar) --> [ganhou],{valida_sujeito(Sujeito);assert(erro(semantico)),!,fail}.
verbo(p,ganharam,[Sujeito],ganhar) --> [ganharam],{valida_sujeito(Sujeito);assert(erro(semantico)),!,fail}.
verbo(s,realizou,[Sujeito],realizar) --> [realizou],{pessoa(Sujeito);assert(erro(semantico)),!,fail}.
verbo(s,entrou,[Sujeito],entrar) --> [entrou],{pessoa(Sujeito);assert(erro(semantico)),!,fail}.
verbo(s,foi,_,ser) --> [foi].
verbo(p,foram,_,ser) --> [foram].

% Filmes
nome(m-s,'No Country for Old Men','No Country for Old Men') --> ['No','Country',for,'Old','Men'].
nome(m-s,'The Golden Compass','The Golden Compass') --> ['The','Golden','Compass'].
nome(m-s,'There Will Be Blood','There Will Be Blood') --> ['There','Will','Be','Blood'].
nome(m-s,'Michael Clayton','Michael Clayton') --> ['Michael','Clayton'].
nome(m-s,'Juno','Juno') --> ['Juno'].
nome(m-s,'The Diving Bell and the Butterfly','The Diving Bell and the Butterfly') -->
    ['The','Divinity','Bell',and,the,'Butterfly'];
        ['The','Divinity','Bell','And','The','Butterfly'].
nome(m-_,'Pirates of the Caribbean','Pirates of the Caribbean') -->
        ['Pirates',of,the,'Caribbean'];['Pirates','Of','The','Caribbean'].
nome(m-_,'Transformers','Transformers') --> ['Transformers'].
nome(m-s,'Sweeney Todd','Sweeney Todd') --> ['Sweeney','Todd'].
nome(m-s,'In the Valley of Elah','In the Valley of Elah') --> 
        ['In',the,'Valley',of,'Elah'];['In','The','Valley','Of','Elah'].
nome(m-s,'Eastern Promises','Eastern Promises') --> ['Eastern','Promises'].
nome(m-s,'Atonement','Atonement') --> ['Atonement'].
nome(m-s,'Elizabeth: The Golden Age','Elizabeth: The Golden Age') --> 
        ['Elizabeth:','The','Golden','Age'].
nome(m-s,'Away from Her','Away from Her') --> ['Away',from,'Her'].
nome(m-s,'La môme','La môme') --> ['La',môme];['La','Môme'].
nome(m-s,'The Savages','The Savages') --> ['The','Savages'].

% Actores
nome(m-s,'Daniel Day-Lewis','Daniel Day-Lewis') --> ['Daniel', 'Day-Lewis'].
nome(m-s,'George Clooney','George Clooney') --> ['George','Clooney'].
nome(m-s,'Johnny Depp','Johnny Depp') --> ['Johnny','Depp'].
nome(m-s,'Tommy Lee Jones','Tommy Lee Jones') --> ['Tommy','Lee','Jones'].
nome(m-s,'Viggo Mortensen','Viggo Mortensen') --> ['Viggo','Mortensen'].
nome(f-s,'Cate Blanchett','Cate Blanchett') --> ['Cate','Blanchett'].
nome(f-s,'Julie Christie','Julie Christie') --> ['Julie','Christie'].
nome(f-s,'Marion Cotillard','Marion Cotillard') --> ['Marion','Cotillard'].
nome(f-s,'Laura Linney','Laura Linney') --> ['Laura','Linney'].
nome(f-s,'Ellen Page','Ellen Page') --> ['Ellen','Page'].

% Realizadores
nome(m-s,'Julian Schnabel','Julian Schnabel') --> ['Julian','Schnabel'].
nome(m-s,'Jason Reitman','Jason Reitman') --> ['Jason','Reitman'].
nome(m-s,'Tony Gilroy','Tony Gilroy') --> ['Tony','Gilroy'].
nome(m-p,'Irmãos Coen','Irmãos Coen') --> [irmãos, 'Coen'];['Irmãos', 'Coen'].
nome(m-s,'Paul Thomas Anderson','Paul Thomas Anderson') --> ['Paul','Thomas','Anderson'].

% Pessoas
nome(m-s,'Michael Fink','Michael Fink') --> ['Michael','Fink'].
nome(m-s,'Bill Westenhofer','Bill Westenhofer') --> ['Bill','Westenhofer'].
nome(m-s,'Ben Morris','Ben Morris') --> ['Ben','Morris'].
nome(m-s,'Trevor Wood','Trevor Wood') --> ['Trevor','Wood'].

% Prémios
nome(m-s,'Melhor Filme','Melhor Filme') --> [melhor,filme].
nome(m-s,'Melhor Realizador','Melhor Realizador') --> [melhor,realizador].
nome(m-s,'Melhor Actor','Melhor Actor') --> [melhor,actor].
nome(f-s,'Melhor Actriz','Melhor Actriz') --> [melhor,actriz].
nome(m-s,'Melhor Actor Secundário','Melhor Actor Secundário') --> [melhor,actor,secundário].
nome(f-s,'Melhor Actriz Secundária','Melhor Actriz Secundária') --> [melhor,actriz,secundária].
nome(m-s,'Melhor Argumento Original','Melhor Argumento Original') --> [melhor,argumento,original].
nome(m-s,'Melhor Argumento Adaptado','Melhor Argumento Adaptado') --> [melhor,argumento,adaptado].
nome(m-s,'Melhor Filme de Animação','Melhor Filme de Animação') --> [melhor,filme,de,animação].
nome(m-s,'Melhor Filme em Lingua Estrangeira','Melhor Filme em Lingua Estrangeira') -->
        [melhor,filme,em,lingua,estrangeira].
nome(f-s,'Melhor Fotografia','Melhor Fotografia') --> [melhor,fotografia].
nome(f-s,'Melhor Direcção Artística','Melhor Direcção Artística') --> [melhor,direcção,artística].
nome(m-s,'Melhor Guarda-Roupa','Melhor Guarda-Roupa') --> [melhor,guarda-roupa].
nome(f-s,'Melhor Edição','Melhor Edição') --> [melhor,edição].
nome(f-s,'Melhor Caracterização','Melhor Caracterização') --> [melhor,caracterização].
nome(m-p,'Melhores Efeitos Visuais','Melhores Efeitos Visuais') --> [melhores,efeitos,visuais].
nome(f-s,'Melhor Montagem de Som','Melhor Montagem de Som') --> [melhor,montagem,de,som].
nome(f-s,'Melhor Mistura de Som','Melhor Mistura de Som') --> [melhor,mistura,de,som].
nome(f-s,'Melhor Banda Sonora','Melhor Banda Sonora') --> [melhor,banda,sonora].
nome(f-s,'Melhor Canção Original','Melhor Canção Original') --> [melhor,canção,original].
nome(m-s,'Melhor Documentário','Melhor Documentário') --> [melhor,documentário].
nome(m-s,'Melhor Documentário em Curta-Metragem','Melhor Documentário em Curta-Metragem') --> 
        [melhor,documentário,em,curta-metragem].
nome(f-s,'Melhor Curta Metragem','Melhor Curta Metragem') --> [melhor,curta-metragem].
nome(f-s,'Melhor Curta Metragem de Animação','Melhor Curta Metragem de Animação') --> 
        [melhor,curta-metragem,de,animação].
nome(m-s,'Óscar Honorário','Óscar Honorário') --> [óscar,honorário].

% Vocabulário geral
nome(m-s,premio,premio) --> [prémio].
nome(m-p,premios,premio) --> [prémios].
nome(m-s,oscar,oscar) --> [óscar].
nome(m-p,oscares,oscar) --> [óscares].
nome(m-p,filmes,filme) --> [filmes].
nome(m-s,filme,filme) --> [filme].
nome(m-p,actores,actor) --> [actores].
nome(m-s,actor,actor) --> [actor].
nome(f-p,actrizes,actor) --> [actrizes].
nome(f-s,actriz,actor) --> [actriz].
nome(f-s,pessoa,pessoa) --> [pessoa].
nome(f-p,pessoas,pessoa) --> [pessoas].
nome(m-s,realizador,realizador) --> [realizador].
nome(m-p,realizadores,realizador) --> [realizadores].
nome(m-s,realizado,realizado) --> [realizado].
nome(m-s,nomeado,nomeado) --> [nomeado].
nome(m-p,nomeados,nomeado) --> [nomeados].
nome(f-s,nomeada,nomeado) --> [nomeada].
nome(f-p,nomeadas,nomeado) --> [nomeadas].
nome(f-p,nomeacoes,nomeacoes) --> [nomeações].

% BASE DE CONHECIMENTO

% Filmes
filme('Juno').
filme('Michael Clayton').
filme('There Will Be Blood').
filme('No Country for Old Men').
filme('The Golden Compass').
filme('The Diving Bell and the Butterfly').
filme('Sweeney Todd').
filme('In the Valley of Elah').
filme('Eastern Promises').
filme('Elizabeth: The Golden Age').
filme('Away from Her').
filme('La môme').
filme('The Savages').

% Actores
actor('George Clooney',_).
actor('Daniel Day-Lewis',_).
actor('Johnny Depp',_).
actor('Tommy Lee Jones',_).
actor('Viggo Mortensen',_).
actor('Cate Blanchett',_).
actor('Julie Christie',_).
actor('Marion Cotillard',_).
actor('Laura Linney',_).
actor('Ellen Page',_).

% Realizadores
realizador('Julian Schnabel').
realizador('Jason Reitman').
realizador('Tony Gilroy').
realizador('Irmãos Coen').
realizador('Paul Thomas Anderson').

% Pessoas envolvidas nos filmes em geral
pessoa('Michael Fink').
pessoa('Bill Westenhofer').
pessoa('Ben Morris').
pessoa('Trevor Wood').
pessoa('John Knoll').
pessoa('Hal Hickel').
pessoa('Charles Gibson').
pessoa('John Frazier').
pessoa('Scott Farrar').
pessoa('Scott Benza').
pessoa('Russell Earl').

pessoa(Sujeito):-
    actor(Sujeito,_);
    realizador(Sujeito).

% ACÇÕES

% Prémios

ganhar('No Country for Old Men','Melhor Filme').

ganhar('Daniel Day-Lewis','Melhor Actor').
 ganhar('There Will Be Blood','Melhor Actor').

ganhar('Irmãos Coen','Melhor Realizador').
 ganhar('No Country for Old Men','Melhor Realizador').

ganhar('The Golden Compass','Melhores Efeitos Visuais').
 ganhar('Michael Fink','Melhores Efeitos Visuais').
 ganhar('Bill Westenhofer','Melhores Efeitos Visuais').
 ganhar('Ben Morris','Melhores Efeitos Visuais').
 ganhar('Trevor Wood','Melhores Efeitos Visuais').

ganhar('Marion Cotillard','Melhor Actriz').
 ganhar('La môme','Melhor Actriz').

% Nomeados

nomeado('Atonement','Melhor Filme').
nomeado('Juno','Melhor Filme').
nomeado('Michael Clayton','Melhor Filme').
nomeado('There Will Be Blood','Melhor Filme').
nomeado('No Country for Old Men','Melhor Filme').

nomeado('Julian Schnabel','Melhor Realizador').
 nomeado('The Diving Bell and the Butterfly','Melhor Realizador').
nomeado('Jason Reitman','Melhor Realizador').
 nomeado('Juno','Melhor Realizador').
nomeado('Tony Gilroy','Melhor Realizador').
 nomeado('Michael Clayton','Melhor Realizador').
nomeado('Irmãos Coen','Melhor Realizador').
 nomeado('No Country for Old Men','Melhor Realizador').
nomeado('Paul Thomas Anderson','Melhor Realizador').
 nomeado('There Will Be Blood','Melhor Realizador').

nomeado('George Clooney','Melhor Actor').
 nomeado('Michael Clayton','Melhor Actor').
nomeado('Daniel Day-Lewis','Melhor Actor').
 nomeado('There Will Be Blood','Melhor Actor').
nomeado('Johnny Depp','Melhor Actor').
 nomeado('Sweeney Todd','Melhor Actor').
nomeado('Tommy Lee Jones','Melhor Actor').
 nomeado('In the Valley of Elah','Melhor Actor').
nomeado('Viggo Mortensen','Melhor Actor').
 nomeado('Eastern Promises','Melhor Actor').

nomeado('The Golden Compass','Melhores Efeitos Visuais').
 nomeado('Michael Fink','Melhores Efeitos Visuais').
 nomeado('Bill Westenhofer','Melhores Efeitos Visuais').
 nomeado('Ben Morris','Melhores Efeitos Visuais').
 nomeado('Trevor Wood','Melhores Efeitos Visuais').
nomeado('Pirates of the Caribbean','Melhores Efeitos Visuais').
 nomeado('John Knoll','Melhores Efeitos Visuais').
 nomeado('Hal Hickel','Melhores Efeitos Visuais').
 nomeado('Charles Gibson','Melhores Efeitos Visuais').
 nomeado('John Frazier','Melhores Efeitos Visuais').
nomeado('Transformers','Melhores Efeitos Visuais').
 nomeado('Scott Farrar','Melhores Efeitos Visuais').
 nomeado('Scott Benza','Melhores Efeitos Visuais').
 nomeado('Russell Earl','Melhores Efeitos Visuais').
 %nomeado('John Frazier','Melhores Efeitos Visuais').

nomeado('Cate Blanchett','Melhor Actriz').
 nomeado('Elizabeth: The Golden Age','Melhor Actriz').
nomeado('Julie Christie','Melhor Actriz').
 nomeado('Away from Her','Melhor Actriz').
nomeado('Marion Cotillard','Melhor Actriz').
 nomeado('La môme','Melhor Actriz').
nomeado('Laura Linney','Melhor Actriz').
 nomeado('The Savages','Melhor Actriz').
nomeado('Ellen Page','Melhor Actriz').
 nomeado('Juno','Melhor Actriz').

nomeacao(Nome,Premio):-nomeado(Nome,Premio).

% Quem fez o quê

realizar('Julian Schnabel','The Diving Bell and the Butterfly').
realizar('Jason Reitman','Juno').
realizar('Tony Gilroy','Michael Clayton').
realizar('Irmãos Coen','No Country for Old Men').
realizar('Paul Thomas Anderson','There Will Be Blood').

realizado(Filme,Realizador):-realizar(Realizador,Filme).

entrar('George Clooney','Michael Clayton').
entrar('Daniel Day-Lewis','There Will Be Blood').
entrar('Johnny Depp','Sweeney Todd').
entrar('Tommy Lee Jones','In the Valley of Elah').
entrar('Viggo Mortensen','Eastern Promises').
entrar('Cate Blanchett','Elizabeth: The Golden Age').
entrar('Julie Christie','Away from Her').
entrar('Marion Cotillard','La môme').
entrar('Laura Linney','The Savages').
entrar('Ellen Page','Juno').