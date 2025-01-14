using Grpc.Core;
using Grpc.Net.Client;
using GrpcServiceDemo;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrpcClientDemo
{




public class HftClient
{
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