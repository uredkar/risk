// See https://aka.ms/new-console-template for more information
using System.Text;
using System;
using RabbitMQ.Client;
using RabbitMQConsumer;

Console.WriteLine("Hello, World!");

var factory = new ConnectionFactory() { HostName = "localhost" }; // Connect to local Docker container
try
{
    using (var connection = await factory.CreateConnectionAsync())
    using (var channel = await connection.CreateChannelAsync())
    {
        await channel.QueueDeclareAsync(queue: "my_queue",
                             durable: false,
                             exclusive: false,
                             autoDelete: false,
                             arguments: null);

        string message = "Hello from RabbitMQ Producer (Async)!";
        var body = Encoding.UTF8.GetBytes(message);
        var props = new BasicProperties
        {
            Persistent = true
        };
        await channel.BasicPublishAsync(exchange: String.Empty,
                             routingKey: "my_queue",
                             mandatory: true,
                             basicProperties: props,
                             body: body);

        Console.WriteLine(" [x] Sent {0}", message);
    }
}
catch (Exception ex)
{
    Console.WriteLine($"An error occurred: {ex.Message}");
}

Console.WriteLine(" Trying to Consume");
await RabbitMQConsumer.Consumer.Execute();

Console.WriteLine(" Press [enter] to exit.");
Console.ReadLine();


