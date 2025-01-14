using Grpc.Core;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace GrpcServiceDemo.Services;

public class PeopleService : People.PeopleBase
{
    public override Task<PeopleList> GetPeople(Empty request, ServerCallContext context)
    {
        var peopleList = new PeopleList
        {
            People =
        {
            new Person { Name = "Alice", Age = 30 },
            new Person { Name = "Bob", Age = 25 },
            new Person { Name = "Charlie", Age = 35 }
        }
        };
        return Task.FromResult(peopleList);
    }

    public override Task<Empty> AddPeople(PeopleList request, ServerCallContext context)
    {
        // Process the list of people 
        foreach (var person in request.People)
        {
            // Log or store the person information
            Console.WriteLine($"Received: {person.Name}, {person.Age}");
        }

        return Task.FromResult(new Empty());
    }
}
