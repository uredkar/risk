using Grpc.Core;
using Grpc.Core.Interceptors;

namespace GrpcServiceDemo
{
    public class LoggingInterceptor : Interceptor
    {
        public override async Task<TResponse> UnaryServerHandler<TRequest, TResponse>(
           TRequest request,
           ServerCallContext context,
           UnaryServerMethod<TRequest, TResponse> continuation)
        {
                Console.WriteLine($"Request: {request}");
                var response = await continuation(request, context);
                Console.WriteLine($"Response: {response}");
                return response;
        }
    }
}
