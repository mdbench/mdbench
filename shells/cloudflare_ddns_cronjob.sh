#!/bin/bash
### This script operates under the assumption you are running it as a root user...###
### ...in conjunction with the Cloudflare service in a directory named scripts...###
### ...and you will need jq installed on the host system (i.e. on OpenWRT, opkg install jq)...###
# Log function to create and update DDNS record succes/failure by date-time group
LOG_FILE="ddnsupdaterlog.txt"
TEMP_FILE="ddnsupdaterlog-tmp.txt"
log_message() {
  # Add the new content to the temp file
  echo "$(date) - $1" > "/root/scripts/$TEMP_FILE"
  # Append the original log file content to the temp file
  echo "$(head -2000 /root/scripts/$LOG_FILE)" >> "/root/scripts/$TEMP_FILE"
  # Replace the original log file with the content of the temp file
  mv "/root/scripts/$TEMP_FILE" "/root/scripts/$LOG_FILE"
  : 'echo "$(date) - $1" >> "/root/scripts/$LOG_FILE"'
}

zoneid=ZONEIDSHOULDGOHERE
dnsrecord=RECORDDOMAINSHOULDGOHERE
cloudflareauthkey=TOKENAUTHKEYSHOULDGOHERE

# Get the current external IP address
# ip=$(curl -s -X GET https://checkip.amazonaws.com)
ip=$(curl -s https://api.ipify.org)

echo "Current IP is $ip"
echo "Zoneid is $zoneid"

# Get DNS Record ID
dnstokenid=$(curl -X GET "https://api.cloudflare.com/client/v4/zones/$zoneid/dns_records?name=$dnsrecord" -H "Authorization: Bearer $cloudflareauthkey" -H "Content-Type: application/json" | jq -r '{"result"}[] | .[0] | .id')
echo "DNS Record ID response is: $dnstokenid"

# Create JSON payload
json=$(jq -c -n --arg ip2 "$ip" '{"content":$ip2}')
json_string="$json"
echo "$json_string"

# Update the record
echo -e "Updating record at URL: https://api.cloudflare.com/client/v4/zones/$zoneid/dns_records/$dnstokenid\n"
response=$(curl -X PATCH "https://api.cloudflare.com/client/v4/zones/$zoneid/dns_records/$dnstokenid" -H "Authorization: Bearer $cloudflareauthkey" -H "Content-Type: application/json" --data "$json_string")

# Finish script by giving user feedback and logging result
echo -e "\nUpdate complete..."
echo -e "\nResult is:\n\n $response"
log_message "$response"
