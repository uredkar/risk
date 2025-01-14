namespace GrpcServiceDemo
{
    public class LoadBalancer
    {
        private readonly List<string> _serverAddresses;
        private int _lastIndex = -1;
        private readonly Dictionary<string, int> _connectionCounts;

        public LoadBalancer(IEnumerable<string> serverAddresses)
        {
            _serverAddresses = serverAddresses.ToList();
            _connectionCounts = _serverAddresses.ToDictionary(addr => addr, addr => 0);
        }

        // Round Robin Strategy
        public string GetServer_RoundRobin()
        {
            _lastIndex = (_lastIndex + 1) % _serverAddresses.Count;
            return _serverAddresses[_lastIndex];
        }

        // Least Connections Strategy
        public string GetServer_LeastConnections()
        {
            return _connectionCounts.OrderBy(kvp => kvp.Value).First().Key;
        }

        // Simulate Connection Tracking
        public void SimulateConnection(string serverAddress)
        {
            _connectionCounts[serverAddress]++;
        }

        public void ReleaseConnection(string serverAddress)
        {
            _connectionCounts[serverAddress] = Math.Max(0, _connectionCounts[serverAddress] - 1);
        }
    }
}
