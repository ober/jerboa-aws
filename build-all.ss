#!chezscheme
;; Build driver: imports all modules to trigger Chez compilation

(import
  ;; Core
  (jerboa-aws creds)
  (jerboa-aws crypto)
  (jerboa-aws sigv4)
  (jerboa-aws uri)
  (jerboa-aws time)
  (jerboa-aws xml)
  (jerboa-aws json)
  (jerboa-aws request)
  (jerboa-aws api)
  (jerboa-aws json-api)
  ;; EC2
  (jerboa-aws ec2 xml)
  (jerboa-aws ec2 params)
  (jerboa-aws ec2 api)
  (jerboa-aws ec2 instances)
  (jerboa-aws ec2 security-groups)
  (jerboa-aws ec2 vpcs)
  (jerboa-aws ec2 subnets)
  (jerboa-aws ec2 volumes)
  (jerboa-aws ec2 snapshots)
  (jerboa-aws ec2 addresses)
  (jerboa-aws ec2 network-interfaces)
  (jerboa-aws ec2 key-pairs)
  (jerboa-aws ec2 images)
  (jerboa-aws ec2 regions)
  (jerboa-aws ec2 tags)
  (jerboa-aws ec2 route-tables)
  (jerboa-aws ec2 internet-gateways)
  (jerboa-aws ec2 nat-gateways)
  (jerboa-aws ec2 launch-templates)
  ;; S3
  (jerboa-aws s3 xml)
  (jerboa-aws s3 api)
  (jerboa-aws s3 buckets)
  (jerboa-aws s3 objects)
  ;; STS
  (jerboa-aws sts api)
  (jerboa-aws sts operations)
  ;; IAM
  (jerboa-aws iam api)
  (jerboa-aws iam users)
  (jerboa-aws iam groups)
  (jerboa-aws iam roles)
  (jerboa-aws iam policies)
  (jerboa-aws iam access-keys)
  ;; Lambda
  (jerboa-aws lambda api)
  (jerboa-aws lambda functions)
  ;; CloudWatch Logs
  (jerboa-aws logs api)
  (jerboa-aws logs operations)
  ;; DynamoDB
  (jerboa-aws dynamodb api)
  (jerboa-aws dynamodb operations)
  ;; SNS
  (jerboa-aws sns api)
  (jerboa-aws sns operations)
  ;; SQS
  (jerboa-aws sqs api)
  (jerboa-aws sqs operations)
  ;; CloudFormation
  (jerboa-aws cfn api)
  (jerboa-aws cfn stacks)
  ;; CloudWatch
  (jerboa-aws cloudwatch api)
  (jerboa-aws cloudwatch operations)
  ;; RDS
  (jerboa-aws rds api)
  (jerboa-aws rds db-instances)
  ;; ELBv2
  (jerboa-aws elbv2 api)
  (jerboa-aws elbv2 operations)
  ;; SSM
  (jerboa-aws ssm api)
  (jerboa-aws ssm operations)
  ;; Compute Optimizer
  (jerboa-aws compute-optimizer api)
  (jerboa-aws compute-optimizer operations)
  ;; Cost Optimization Hub
  (jerboa-aws cost-optimization-hub api)
  (jerboa-aws cost-optimization-hub operations)
  ;; CLI
  (jerboa-aws cli format)
  (jerboa-aws cli main)
)
