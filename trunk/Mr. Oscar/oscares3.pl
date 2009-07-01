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
                ( (erro(semantico),write('Erro semântico!'),assert(resultado('Erro semântico')));
                  (write('Erro sintático!'),assert(resultado('Erro sintático')))
                )
        ).

verifica_frase -->
    verifica_frase_afirmativa;
    verifica_frase_interrogativa.

verifica_frase_interrogativa -->
    sintagma_interrogativo(G-N,Pron,TipoSuj),
    {Pron==quem,N=s;N=_},
    sintagma_verbal(G-N,_,Accao,Objecto),
    {resposta_interrogacao(Pron,TipoSuj,Accao,Objecto)}.

verifica_frase_afirmativa -->
    sintagma_nominal(G-N,Sujeito),
    sintagma_verbal(G-N,Sujeito,Accao,Objecto),
    {resposta(Sujeito,Accao,Objecto)}.

sintagma_interrogativo(G-N,Pron,TipoSuj) -->
    pron_int(_,Pron),{Pron=qual;Pron=que},
    sintagma_nominal_int(G-N,TipoSuj,Pron).

sintagma_interrogativo(_,Pron,TipoSuj) -->
    pron_int(_,Pron),
    {Pron=quem,TipoSuj=pessoa}.

sintagma_nominal_int(G-N,Sujeito,Pron) -->
    det(G-N),
    nome(G-N,Sujeito).

sintagma_nominal_int(G-N,Sujeito,Pron) -->
    det(G-N),
    nome(G-N,Sujeito),
    pron(_,que),{Pron=qual}.

sintagma_nominal_int(G-N,Sujeito,Pron) -->
    nome(G-N,Sujeito).

sintagma_nominal(_-p,[Sujeito|R]) -->
     sintagma_nominal1(_,Sujeito),[e],
     sintagma_nominal(_,R).

sintagma_nominal(G-N,[Sujeito]) -->
     sintagma_nominal1(G-N,Sujeito).

sintagma_nominal1(G-N,Sujeito) -->
    det(G-N),
    nome(G-N,Sujeito).

sintagma_nominal1(G-N,Sujeito) -->
    nome(G-N,Sujeito).

sintagma_verbal(G-N,_,Accao,Objecto) -->
    verbo(N,_,ser),!,
    nome(G-N,Accao),
    sintagma_prep(Objecto).

sintagma_verbal(_-N,Sujeito,Accao,Objecto) -->
    verbo(N,Sujeito,Accao),
    sintagma_nominal(_,_),
    sintagma_prep(Objecto).

sintagma_verbal(_-N,Sujeito,Accao,Objecto) -->
    verbo(N,Sujeito,Accao),
    sintagma_nominal(_,[Objecto]).

sintagma_verbal(_-N,Sujeito,Accao,Objecto) -->
    verbo(N,Sujeito,Accao),
    sintagma_prep(Objecto).

sintagma_prep(Objecto) -->
    prep(G-N),
    sintagma_nominal(G-N,[Objecto]).

% RESPOSTAS

valida_factos([Sujeito|R],Accao,Objecto):-
    Facto=..[Accao,Sujeito,Objecto],
    Facto,
    valida_factos(R,Accao,Objecto).

valida_factos([Sujeito],Accao,Objecto):-
    Facto=..[Accao,Sujeito,Objecto],
    Facto.

resposta(Sujeito,Accao,Objecto):-
    (valida_factos(Sujeito,Accao,Objecto),
    write('Sim'),assert(resultado('Sim')));
    write('Não'),assert(resultado('Não')).

resposta_interrogacao(Pron,TipoSuj,Accao,Objecto):-
    Facto=..[Accao,Sujeito,Objecto],
    Validacao=..[TipoSuj,Sujeito],
    findall(Sujeito,(Facto,Validacao),Resultados),
    %Pron==quanto,length(Resultados,Len),write(Len);
    write(Resultados),converte_em_resultados(Resultados);
    write('Sem Resultados'),assert(resultado('Sem Resultados')).

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

pron(_,que) --> [que].

pron_int(s-_,quem,qual) --> ['Quem'];[quem].
pron_int(s-_,qual,qual) --> ['Qual'];[qual].
pron_int(p-_,quais,qual) --> ['Quais'];[quais].
pron_int(_,que) --> ['Que'];[que].
pron_int(p-m,quantos,quant) --> ['Quantos'];[quantos].
pron_int(p-f,quantas,quant) --> ['Quantas'];[quantas].

verbo(s,[Sujeito],ganhar) --> [ganhou],{valida_sujeito(Sujeito);assert(erro(semantico)),!,fail}.
verbo(p,[Sujeito],ganhar) --> [ganharam],{valida_sujeito(Sujeito);assert(erro(semantico)),!,fail}.
verbo(s,[Sujeito],realizar) --> [realizou],{pessoa(Sujeito);assert(erro(semantico)),!,fail}.
verbo(s,[Sujeito],entrar) --> [entrou],{pessoa(Sujeito);assert(erro(semantico)),!,fail}.
verbo(s,_,ser) --> [foi].
verbo(p,_,ser) --> [foram].

% Filmes
nome(m-s,'No Country for Old Men','No Country for Old Men') --> ['No','Country',for,'Old','Men'].
nome(m-s,'The Golden Compass','The Golden Compass') --> ['The','Golden','Compass'].
nome(m-s,'There Will Be Blood','There Will Be Blood') --> ['There','Will','Be','Blood'].
nome(m-s,'Michael Clayton','Michael Clayton') --> ['Michael','Clayton'].
nome(m-s,'Juno','Juno') --> ['Juno'].
nome(m-s,'The Diving Bell and the Butterfly','The Diving Bell and the Butterfly') -->
    ['The','Divinity','Bell',and,the,'Butterfly'];
    ['The','Divinity','Bell','And','The','Butterfly'].
nome(m-_,'Pirates of the Caribbean','Pirates of the Caribbean') --> ['Pirates',of,the,'Caribbean'];['Pirates','Of','The','Caribbean'].
nome(m-_,'Transformers','Transformers') --> ['Transformers'].
nome(m-s,'Sweeney Todd','Sweeney Todd') --> ['Sweeney','Todd'].
nome(m-s,'In the Valley of Elah','In the Valley of Elah') --> ['In',the,'Valley',of,'Elah'];['In','The','Valley','Of','Elah'].
nome(m-s,'Eastern Promises','Eastern Promises') --> ['Eastern','Promises'].
nome(m-s,'Atonement','Atonement') --> ['Atonement'].
nome(m-s,'Elizabeth: The Golden Age','Elizabeth: The Golden Age') --> ['Elizabeth:','The','Golden','Age'].
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
nome(m-s,'Melhor Filme em Lingua Estrangeira','Melhor Filme em Lingua Estrangeira') --> [melhor,filme,em,lingua,estrangeira].
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
nome(m-s,'Melhor Documentário em Curta-Metragem','Melhor Documentário em Curta-Metragem') --> [melhor,documentário,em,curta-metragem].
nome(f-s,'Melhor Curta Metragem','Melhor Curta Metragem') --> [melhor,curta-metragem].
nome(f-s,'Melhor Curta Metragem de Animação','Melhor Curta Metragem de Animação') --> [melhor,curta-metragem,de,animação].
nome(m-s,'Óscar Honorário','Óscar Honorário') --> [óscar,honorário].

% Vocabulário geral
nome(m-s,premio) --> [prémio].
nome(m-s,oscar) --> [óscar].
nome(m-p,filme) --> [filmes].
nome(m-s,filme) --> [filme].
nome(m-p,actor) --> [actores].
nome(m-s,actor) --> [actor].
nome(f-p,actor) --> [actrizes].
nome(f-s,actor) --> [actriz].
nome(f-s,pessoa) --> [pessoa].
nome(f-p,pessoa) --> [pessoas].
nome(m-s,realizador) --> [realizador].
nome(m-p,realizador) --> [realizadores].
nome(m-s,nomeado) --> [nomeado].
nome(m-p,nomeado) --> [nomeados].
nome(f-s,nomeado) --> [nomeada].
nome(f-p,nomeado) --> [nomeadas].
nome(f-p,nomeacoes) --> [nomeações].

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
actor('George Clooney').
actor('Daniel Day-Lewis').
actor('Johnny Depp').
actor('Tommy Lee Jones').
actor('Viggo Mortensen').
actor('Cate Blanchett').
actor('Julie Christie').
actor('Marion Cotillard').
actor('Laura Linney').
actor('Ellen Page').

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
    actor(Sujeito);
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