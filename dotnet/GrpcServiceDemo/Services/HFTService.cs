using Grpc.Core;
using GrpcServiceDemo.Services;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.Collections.Concurrent;
using System.Text.Json;
using System.Threading;

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


        private static readonly ConcurrentDictionary<string, (int Progress, string? CallbackUrl)> TaskStates = new();

        public override async Task StartProcess(LongRunningRequest request, IServerStreamWriter<LongRunningResponse> responseStream, ServerCallContext context)
        {
            var taskId = request.TaskId;
            TaskStates.TryAdd(taskId, (0, null));
            Console.WriteLine($"StartProcess Task ID : {taskId}");
            for (int i = 0; i <= 100; i += 10)
            {
                if (context.CancellationToken.IsCancellationRequested)
                {
                    Console.WriteLine($"Task {taskId} cancelled.");
                    TaskStates[taskId] = (i, TaskStates[taskId].CallbackUrl);
                    return;
                }

                try
                {
                    // Simulate work
                    await Task.Delay(500);

                    // Update task progress
                    TaskStates[taskId] = (i, TaskStates[taskId].CallbackUrl);

                    // Send progress to the client
                    await responseStream.WriteAsync(new LongRunningResponse
                    {
                        Status = "In Progress",
                        Progress = i
                    });
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Error in task {taskId}: {ex.Message}");
                    // Retry logic can be implemented here or by the client
                }
            }

            // Mark task as complete
            TaskStates[taskId] = (100, TaskStates[taskId].CallbackUrl);

            // Notify client via callback if registered
            if (TaskStates[taskId].CallbackUrl != null)
            {
                await NotifyClientAsync(taskId, TaskStates[taskId].CallbackUrl);
            }

            await responseStream.WriteAsync(new LongRunningResponse
            {
                Status = "Completed",
                Progress = 100
            });
        }

        public override Task<CallbackResponse> RegisterCallback(CallbackRequest request, ServerCallContext context)
        {
            /*
            if (TaskStates.ContainsKey(request.TaskId))
            {
                TaskStates[request.TaskId] = (TaskStates[request.TaskId].Progress, request.CallbackUrl);
                return Task.FromResult(new CallbackResponse { Message = "Callback registered successfully." });
            }
            */

            //return Task.FromResult(new CallbackResponse { Message = "Task not found." });
            var taskId = request.TaskId;
            var callbackUrl = request.CallbackUrl;
            if (!TaskStates.TryGetValue(taskId, out var taskState))
            {
                // Task doesn't exist yet; initialize it with default progress
                TaskStates.TryAdd(taskId, (0, callbackUrl));
                Console.WriteLine($"Task {taskId} registered with callback URL: {callbackUrl}");
            }
            else
            {
                // Task already exists; update the callback URL if necessary
                TaskStates[taskId] = (taskState.Progress, callbackUrl);
                Console.WriteLine($"Task {taskId} callback URL updated to: {callbackUrl}");
            }
            return Task.FromResult(new CallbackResponse { Message = "Callback registered successfully." });
        }

        private static async Task NotifyClientAsync(string taskId, string callbackUrl)
        {
            using var httpClient = new HttpClient();
            var content = new StringContent(JsonSerializer.Serialize(new { TaskId = taskId, Status = "Completed" }));
            try
            {
                var response = await httpClient.PostAsync(callbackUrl, content);
                if (!response.IsSuccessStatusCode)
                {
                    Console.WriteLine($"Callback to {callbackUrl} failed with status: {response.StatusCode}");
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine($"Error notifying client: {ex.Message}");
            }
        }

    }
}