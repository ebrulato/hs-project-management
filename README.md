# hs-project-management

The purpose of this project is to explore how to implement a CQRS & Event Sourcing µ-service with Haskell, and to see the result of a such implementation :) The intent is not to provide a reusable framework, but to learn by the practice, and maybe to give some ideas to other people. Sometimes my code could be very naive for a Haskell specialist, but if I try to improve myself, I also want to understand my code. One of my purpose is that the result should be simple to understand even if you are not familiar with this language. 

But the main purpose of all that stuff is to have some fun ;)

## Why Haskell ?

Well, I think that this language is wonderful, I can try to give a list of features, but I prefer to explain that the main reason is that it makes me extremly confident with my code. I never feel that with any other language (except Elm, but it is another story)

## CQRS & Event Sourcing... µ-service... 

A lot of buzz words... but a lot of good ideas too :)
I think that the *CQRS* concept is very interresting, and that the *event sourcing* is too often considered as difficult to implement. I will try to give here, in the future, some details about these concepts, and I will try to demistify them If I could.

## Ok but Why "Project Managment" ? 

It is very simple and we can imagine a lot of features that everyboy could understand.
It gives also the possibility to check some interresting kind of evolutions on the Business Features.

# Status of the project

## Command

|Component|Status|Description|Comment|
|---------|------|-----------|-------|
| Logger | TODO | Used to log the message for audit purpose |
| Port | TODO | Used to decode the requests | Servant implementation. 
| Filter | TODO | Used to check the request parameters | As I will use Servant, this part should very simple and focus on the values of the parameters.
| Domain Event Dispatcher| TODO | Used to dispatch the domain events to the Projections Updaters and to the external Adapter. It also have to store the Domain Events in the Domain Event DB. It is a key component for the coherence of the whole system. |
| Domain Event DB | A in-memory version. | Used to store the Domain Events. Very usefull to make the Event Sourcing part of the project, and to add some new projections. | A in-memory version for test purpose and another one based on PostgreSQL ?  
| Command Handler | TODO | Used to orchestrate the Aggregate Repository, the and to perform the Commands on the Aggregates. |
| Aggregate Repository | something | Used to provides a managed access to the Aggregates. | A in-memory version, based on the Domain Event DB, and maybe in the future on a managed Redis (with hedis ?)
| Aggregate | first samples | Implements the Domain Aggregates, and generates Domain Event for the records... | today a very naive version to describe a project.
| Adapter | TODO | have to filter the Domain Events for other systems and to guarantee that the messages will be delivered | We need to have at least another system :(
| Projection Updater | TODO | Used to organize the data for some business requirements that can not be provided by the Aggregate Repository. |
| Event Sourcing | the begining | Used to reconstruct an Aggregate based only on its Domain Events. | The current work on this part is a first POC. But it works. I have to improve the error management in the future, but this version try to check the integrity of the Events. 
| Bus | TODO | The pipeline used to manage each request safely | It could be a lot of thing, but in my simple version I think we could consider that it will be implemented with the Servant library. 

## Query

|Component|Status|Description|Comment|
|---------|------|-----------|-------|
| Logger | TODO | Used to log the message for audit purpose |
| Port | TODO | Used to decode the requests. | Servant implementation. 
| Filter | TODO | Used to check the request parameters | As I will use Servant, this part should very simple and focus on the values of the parameters.
| Cache | TODO | If necessary... | Often the worst idea. But why not a simple in-memory version? |
| Query Hanler | TODO | well, the implementation of the query | 
| Aggregate Reader | TODO | A version of the Aggregate Repository which support only a read version. | 
| Projection DB | TODO | Used to store the data for special business requirements that are not supported by the Aggregate Reader. | 

## Security

Security should be applied at different levels.
As I will use Servant, I could imagine a control at the PORT level. 
But as I will try to deploy all this stuff in the Cloud (with Google Cloud Run) I may have a look at the Google's behaviors. 

# Installation

The module has been developed in Haskell, so you must follow the installation of [***stack***](https://docs.haskellstack.org/en/stable/README/) ;)

## Test the module

Theses tests cover only the translation part of the module.
The CLI and the Web Service (Servant) are not covered.

> $> stack test

