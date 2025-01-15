using Grpc.Core;
using Grpc.Net.Client;
using GrpcServiceDemo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;

namespace GrpcClientDemo
{




public class HftClient
{
    public static async Task LongRunningProcess()
    {
        var channel = GrpcChannel.ForAddress("https://localhost:5001");
        var client = new HftService.HftServiceClient(channel);

        // Register callback
        var callbackResponse = client.RegisterCallback(new CallbackRequest
        {
            TaskId = "12345",
            CallbackUrl = "http://localhost:5002/callback" // Local endpoint to receive notifications
        });
        Console.WriteLine($"Callback registration response: {callbackResponse.Message}");

        // Start the process and stream progress
        var callbackListenerTask = Task.Run(async () =>
        {

            // Local HTTP listener to simulate receiving callback
            var httpListener = new HttpListener();
            httpListener.Prefixes.Add("http://localhost:5002/callback/");
            httpListener.Start();
            Console.WriteLine("Listening for callbacks...");

            while (true)
            {
                var context = await httpListener.GetContextAsync();
                var requestBody = new StreamReader(context.Request.InputStream).ReadToEnd();
                Console.WriteLine($"Received callback: {requestBody}");

                var response = context.Response;
                response.StatusCode = 200;
                await response.OutputStream.WriteAsync(Encoding.UTF8.GetBytes("Callback processed"));
                response.Close();
                //break;
            }
        });
        var serverResponseTask = Task.Run(async () =>
        {
            var responseStream = client.StartProcess(new LongRunningRequest { TaskId = "12345" });

            await foreach (var response in responseStream.ResponseStream.ReadAllAsync())
            {
                Console.WriteLine($"Status: {response.Status}, Progress: {response.Progress}%");
            }
        });

        await Task.WhenAll(serverResponseTask,callbackListenerTask);
    }

    public static async Task Execute()
    {
        // Connect to the server
        using var channel = GrpcChannel.ForAddress("https://localhost:5001");
        var client = new HftService.HftServiceClient(channel);

        // Subscribe to market data for a list of symbols
        var request = new SubscribeRequest
        {
            ClientId = "HftClient1",
            Symbols = { "AAPL", "GOOGL", "MSFT" }
        };

        using var call = client.StreamHftData(request);

        try
        {
            await foreach (var data in call.ResponseStream.ReadAllAsync())
            {
                Console.WriteLine($"Source: {data.SourceId}, Symbol: {data.MarketData.Symbol}");
                Console.WriteLine($"Bid: {data.MarketData.Bid.Price}, Ask: {data.MarketData.Ask.Price}");
                Console.WriteLine($"Trade: {data.TradeExecutions[0].TradeId}, Price: {data.TradeExecutions[0].Price}");
                Console.WriteLine(new string('-', 50));
            }
        }
        catch (RpcException ex)
        {
            Console.WriteLine($"RPC failed: {ex.Status}");
        }
    }
}

}