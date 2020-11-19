from rest_framework import serializers
from . import models

class PostCategorySerializer (serializers.ModelSerializer):
    class Meta:
        model = models.PostCategory
        fields = [ 
            "id" ,
            "category_name",
            "color",
            # "description",
            # "created",
        ]

class SerieSerializer (serializers.ModelSerializer):
    class Meta:
        model = models.Serie
        fields = [ 
            "id",
            "serie_name",
            "description",
            "created",
        ]

class PostSerializer (serializers.ModelSerializer):
    class Meta:
        model = models.Post
        fields = [ 
            "id", 
            "title",
            "text",
            "categories",
            "serie",
            "published",
            "created",
            "updated"
        ]

    def fix_text(self, validated_data):
        text = validated_data.get("text", "")
        validated_data["text"] = text.replace("\r", "")
        return validated_data

    def update(self, instance, validated_data):
        validated_data = self.fix_text(validated_data)
        return super().update(instance, validated_data)

    def create(self, validated_data):
        validated_data = self.fix_text(validated_data)
        return super().create(validated_data)
    
    def to_representation(self, instance):
        ret = super().to_representation(instance)
        categories = []
        for category_id in ret.get("categories"):
            category = models.PostCategory.objects.get(pk=category_id)
            category_serializer = PostCategorySerializer()
            categories.append(category_serializer.to_representation(category))
        ret["categories"] = categories
        return ret

class StrippedPostSerializer(serializers.ModelSerializer):
    categories = PostCategorySerializer(many=True, read_only=True)

    class Meta:
        model = models.Post
        fields = [ 
            "id", 
            "title",
            "categories",
            "serie",
            "published",
            "created",
        ]



