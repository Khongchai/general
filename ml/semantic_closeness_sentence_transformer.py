from sentence_transformers import SentenceTransformer
import torch

def rank_widgets_by_tags(user_query, widget_data):
    model = SentenceTransformer('all-MiniLM-L6-v2')

    # 1. Prepare strings and IDs
    widget_tags_strings = [", ".join(w['tags']) for w in widget_data]
    
    # 2. Generate Embeddings
    tag_embeddings = model.encode(widget_tags_strings, convert_to_tensor=True)
    query_embedding = model.encode([user_query], convert_to_tensor=True)

    # 3. Calculate Similarity
    # result is a 1D tensor of scores: [score1, score2, score3]
    similarities = model.similarity(query_embedding, tag_embeddings)[0]

    # 4. Rank Indices (Highest score first)
    ranked_indices = torch.argsort(similarities, descending=True)
    
    return ranked_indices, similarities

# --- WIDGET DATA ---
widgets = [
    {"id": "TVLLeaderboard", "tags": ["TVL", "leaderboard"]},
    {"id": "CryptoProjectMindshareLeaderboard", "tags": ["crypto", "mindshare", "leaderboard"]},
    {"id": "CryptoProjectCard", "tags": ["crypto", "detail"]}
]

if __name__ == "__main__":
    user_input = "Show me bitcoin price right now"
    
    indices, scores = rank_widgets_by_tags(user_input, widgets)

    print(f"Query: \"{user_input}\"\n")
    print(f"{'Rank':<5} | {'Widget ID':<35} | {'Score':<6}")
    print("-" * 50)

    for rank, idx in enumerate(indices):
        widget = widgets[idx]
        score = scores[idx].item()
        print(f"#{rank+1:<4} | {widget['id']:<35} | {score:.4f}")