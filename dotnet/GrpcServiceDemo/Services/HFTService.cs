using Grpc.Core;
using GrpcServiceDemo.Services;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;


namespace GrpcServiceDemo.Services
{

public class HftServiceImpl : HftService.HftServiceBase
{
    public override async Task StreamHftData(
        SubscribeRequest request,
        IServerStreamWriter<HftData> responseStream,
        ServerCallContext context)
    {
        // Simulated data for demonstration
        var symbols = request.Symbols;

        for (int i = 0; i < 10; i++)
        {
            foreach (var symbol in symbols)
            {
                var hftData = new HftData
                {
                    SourceId = "NYSE",
                    MarketData = new MarketData
                    {
                        Symbol = symbol,
                        Bid = new PriceLevel { Price = 135.67 + i, Volume = (ulong)(1000 + i), Count = 5 },
                        Ask = new PriceLevel { Price = 135.70 + i, Volume = (ulong)(1500 + i), Count = 6 },
                        OrderBook =
                        {
                            new OrderBookLevel
                            {
                                Bid = new PriceLevel { Price = 135.65 + i, Volume = (ulong)(800 + i), Count = 3 },
                                Ask = new PriceLevel { Price = 135.75 + i, Volume = (ulong)(2000 + i), Count = 8 }
                            }
                        }
                    },
                    OrderUpdates =
                    {
                        new OrderUpdate
                        {
                            OrderId = $"Order{i}",
                            Symbol = symbol,
                            OrderType = OrderType.Limit,
                            Action = OrderAction.Insert,
                            Price = 135.68 + i,
                            Volume = (ulong)(100 + i),
                            Timestamp = (ulong)(DateTime.UtcNow.Ticks)
                        }
                    },
                    TradeExecutions =
                    {
                        new Trade
                        {
                            TradeId = $"Trade{i}",
                            Symbol = symbol,
                            Price = 135.69 + i,
                            Volume = (ulong)(500 + i),
                            BuyerId = "TraderX",
                            SellerId = "TraderY",
                            Timestamp = (ulong)(DateTime.UtcNow.Ticks)
                        }
                    },
                    Timestamp = (ulong)(DateTime.UtcNow.Ticks)
                };

                await responseStream.WriteAsync(hftData);
                await Task.Delay(100); // Simulate latency
            }
        }
    }
}

}